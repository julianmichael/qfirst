def get_abst_question_slots(slots):
    def replace_for_slot(slot, old, new):
        return new if slots[slot] == old else slots[slot]
    is_passive = "pastParticiple" in slots["verb"] and (
        "be" in slots["verb"] or
        "is" in slots["aux"] or
        "was" in slots["aux"])
    return {
        "abst-wh": replace_for_slot("wh", "who", "what"),
        "abst-subj": replace_for_slot("subj", "someone", "something"),
        "abst-verb": "verb[pss]" if is_passive else "verb",
        "abst-obj": replace_for_slot("obj", "someone", "something"),
        "abst-prep": "_" if slots["prep"] == "_" else "<prep>",
        "abst-obj2": replace_for_slot("obj2", "someone", "something")
    }
