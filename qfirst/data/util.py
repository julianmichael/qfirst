from typing import NamedTuple

# used for normal slots and abstract slots
def get_slot_label_namespace(slot_name: str) -> str:
    return "slot_%s_labels" % slot_name
