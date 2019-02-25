from typing import NamedTuple, Dict, Any
import atexit
import json
import logging
import os
import tempfile
import tarfile
import shutil

from torch.nn import Module

from allennlp.common.checks import ConfigurationError
from allennlp.common.file_utils import cached_path
from allennlp.common.params import Params, unflatten, with_fallback, parse_overrides
from allennlp.models.model import Model, _DEFAULT_WEIGHTS
from allennlp.models.archival import Archive

logger = logging.getLogger(__name__)  # pylint: disable=invalid-name

def load_archive_from_folder(path: str,
                             cuda_device: int = -1,
                             overrides: str = "",
                             weights_file: str = None) -> Archive:
    # redirect to the cache, if necessary
    resolved_archive_file = cached_path(archive_file)

    if resolved_archive_file == archive_file:
        logger.info(f"loading archive file {archive_file}")
    else:
        logger.info(f"loading archive file {archive_file} from cache at {resolved_archive_file}")

    if os.path.isdir(resolved_archive_file):
        serialization_dir = resolved_archive_file
    else:
        # Extract archive to temp dir
        tempdir = tempfile.mkdtemp()
        logger.info(f"extracting archive file {resolved_archive_file} to temp dir {tempdir}")
        with tarfile.open(resolved_archive_file, 'r:gz') as archive:
            archive.extractall(tempdir)
        # Postpone cleanup until exit in case the unarchived contents are needed outside
        # this function.
        atexit.register(_cleanup_archive_dir, tempdir)

        serialization_dir = tempdir

    # Check for supplemental files in archive
    fta_filename = os.path.join(serialization_dir, _FTA_NAME)
    if os.path.exists(fta_filename):
        with open(fta_filename, 'r') as fta_file:
            files_to_archive = json.loads(fta_file.read())

        # Add these replacements to overrides
        replacements_dict: Dict[str, Any] = {}
        for key, _ in files_to_archive.items():
            if key.startswith("/"):
                replacement_filename = key
            else:
                replacement_filename = os.path.join(serialization_dir, f"fta/{key}")
            replacements_dict[key] = replacement_filename

        overrides_dict = parse_overrides(overrides)
        combined_dict = with_fallback(preferred=unflatten(replacements_dict), fallback=overrides_dict)
        overrides = json.dumps(combined_dict)

    # Load config
    config = Params.from_file(os.path.join(serialization_dir, CONFIG_NAME), overrides)
    config.loading_from_archive = True

    if weights_file:
        weights_path = weights_file
    else:
        weights_path = os.path.join(serialization_dir, _WEIGHTS_NAME)

    # Instantiate model. Use a duplicate of the config, as it will get consumed.
    model = Model.load(config.duplicate(),
                       weights_file=weights_path,
                       serialization_dir=serialization_dir,
                       cuda_device=cuda_device)

    return Archive(model=model, config=config)
