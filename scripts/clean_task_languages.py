#!/usr/bin/env python3
"""Clean bogus/mismatched language slugs in task frontmatter.

Three passes:
  1. Auto-remap slug variants whose normalized form matches an existing page.
  2. Apply hand-curated remaps for known variants normalization misses.
  3. Remove clearly-bogus entries (meta words, sentence fragments parsed from
     code comments or function signatures).

Entries that don't fall into any bucket are left alone — they're plausibly
real languages that just don't have a page yet.
"""
from __future__ import annotations

import re
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
LANGS_DIR = ROOT / "content" / "languages"
TASKS_DIR = ROOT / "content" / "tasks"


def norm(s: str) -> str:
    return re.sub(r"[^a-z0-9]", "", s.lower())


EXPLICIT_REMAP = {
    "c_plus_plus": "cpp",
    "c++": "cpp",
    "c#": "csharp",
    "visual_basic_dotnet": "visual_basic_.net",
    "visual_basic_dot_net": "visual_basic_.net",
    "vbnet": "visual_basic_.net",
    "sinclair_zx_spectrum_basic": "zx_spectrum_basic",
    "visual_basic_for_applications_on_excel": "vba",
    "vba_excel": "vba",
    "ubasic": "ubasic_4th",
}

EXPLICIT_REMOVE = {
    "related_tasks",
    "related_task",
    "executing",
    "alternative_implementation",
    "demo",
    "end",
    "functions",
    "initialize",
    "initialization",
    "main_loop",
    "subroutines",
    "as_a_statement",
    "named_arguments",
    "detail",
    "solve",
    "reason",
    "small",
    "common",
    "bob",
    "x",
    "tr",
    "flex",
    "shell",
    "progress",
    "web",
    "mond",
    "bbc",
    "commodore",
    "inform",
    "version_2",
    "vvvvvv",
    "nirod",
    "swym",
    "netrex",
    "bacom",
    "generic_version",
    "simple_version",
    "insert_some_data",
    "view_the_data",
    "sample_using",
    "functional",
    "tail_recursive",
    "processing_of_input",
    "fpr",
    "k4",
    "stern_brocot_sequence",
    "start_vector",
    "code_block_nim",
    "12_19_16_aev",
    "7_27_16_aev",
    "value_stores_a_string",
    "major_constants",
    "possible_values_in_uint32",
    "ttl_plot_title",
    "recursive_pt_plotting",
    "psz_picture_size",
    "norm_q_5_47722557505166",
    "q1_q2_q2_q1",
    "os_x_sha256sum",
    "taskavast_me_hearties",
    "char_c_void",
    "char_char_void",
    "unichar_char_void",
    "unichar_unichar_void",
    "int_index_void",
    "int_value_void",
    "int_j_void",
    "nibble_in_nibble_void",
    "nibble_nibble_void",
    "range_next_range_void",
    "unirange_unirange_void",
    "lint_m_void",
    "vertex_this_vertex_void",
    "permelemlist_candidate_void",
    "combdatalist_recombination_void",
    "1_cards",
    "1_false",
    "1_true",
    "2_no_title_in_this_version",
    "3_primarily_range_is_changed",
    "january_1901",
    "march_1901",
    "left_rect",
    "mid_rect",
    "right_rect",
    "simpson",
    "trapezium",
    "wikipedia_data",
    "b_column_vector_mx1",
    "h_compressed_matrix_nxm",
    "material_materials_marble",
    "opacity_1_0",
    "selection_of_colors",
    "selection_of_materials",
    "as_the_fitness_function",
    "if_always_switch",
    "if_we_never_switch",
    "if_we_randomly_switch",
    "inputstrings_0_the_fourth",
    "inputstrings_1_assignment",
    "newseq_inputstrings_3",
    "plotting_3_kpf_pictures",
    "t_ttttttttttttttttttt",
    "tt_t_tttt",
    "ttttttttttt_tt",
    "y_yy_y_y_yyy_y_yyy_y_y",
    "ord_order_fn_file_name_ttl_plot_title_clr_color",
    "plotting_sierpinski_triangle_aev_4_1_17",
    "10_numbers",
    "100_numbers",
    "1000_numbers",
    "10000_numbers",
    "100000_numbers",
    "1000000_numbers",
    "10000000_numbers",
}


def is_bogus(slug: str) -> bool:
    """Heuristic: entries that look like code/comment fragments, not language names."""
    if slug in EXPLICIT_REMOVE:
        return True
    if len(slug) > 25:
        return True
    if slug.count("_") >= 4:
        return True
    # Ends with _void, _fn, _ptr — looks like a C-style function signature
    if re.search(r"_(void|fn|ptr|char|int|long|float|double|bool)(_|$)", slug):
        return True
    # Starts with a digit run followed by _ (e.g., "1000_numbers", "4_order_from_randomness")
    if re.match(r"^\d+_", slug):
        return True
    # English sentence fragments: common starters
    if re.match(r"^(if_|the_|to_|that_|does_|can_|for_|see_|as_the_|selection_|list_of_|size_|figure_|up_|axis_|easier_|number_of_|inputstrings_|newseq_|permelemlist_|combdatalist_|vertexroute_|vertex_|material_|opacity_|range_|int_|char_|unichar_|nibble_|lint_|plotting_|animation_|continue_|direction_|no_ttl_|no_title_|plotpoly|plotpolya|pspirals|y_yy|require_|display_|dflg_|generate_|gpkronfractal|kpf_gp|pf_plot|where_|create_|remove_|add_|field_|presents_|pressing_|cells_|dense_|fractional_|modified_|ordinal_|standard_|ord_|x1_|mo_tu_|probchoice|2_no_|3_primarily|tt_t_|ttttttttttt|t_tttttttttttttttttttt|20_members|30_members|embedded_c_|export_|import_|sum_|hamming_|http_|python_|return_|first_class|can_also|a_subroutine|does_however)", slug):
        return True
    # Non-ASCII blobs (transliterated from other scripts)
    if re.search(r"_(ச|ந|ப|க|த|எ|இ|ல|ம|ற|வ|ய|ச_|ட|ர)", slug):
        return True
    return False


def main() -> int:
    existing = {p.stem for p in LANGS_DIR.glob("*.md")}
    norm_to_slug: dict[str, list[str]] = {}
    for s in existing:
        norm_to_slug.setdefault(norm(s), []).append(s)

    remap_stats: Counter[str] = Counter()
    remove_stats: Counter[str] = Counter()
    files_changed = 0

    for task_path in sorted(TASKS_DIR.glob("*.md")):
        text = task_path.read_text()
        fm_match = re.search(r"^(\+\+\+\n)(.*?)(\n\+\+\+)", text, re.DOTALL | re.MULTILINE)
        if not fm_match:
            continue
        fm = fm_match.group(2)
        langs_match = re.search(r"(languages\s*=\s*\[)([^\]]*)(\])", fm, re.DOTALL)
        if not langs_match:
            continue
        langs_inner = langs_match.group(2)
        slugs = re.findall(r'"([^"]+)"', langs_inner)

        new_slugs: list[str] = []
        seen: set[str] = set()
        changed = False
        for s in slugs:
            if s in existing:
                target = s
            elif s in EXPLICIT_REMAP:
                target = EXPLICIT_REMAP[s]
                remap_stats[f"{s} -> {target}"] += 1
                changed = True
            elif norm(s) in norm_to_slug and len(norm_to_slug[norm(s)]) == 1:
                target = norm_to_slug[norm(s)][0]
                remap_stats[f"{s} -> {target}"] += 1
                changed = True
            elif is_bogus(s):
                remove_stats[s] += 1
                changed = True
                continue
            else:
                target = s  # leave alone
            if target not in seen:
                seen.add(target)
                new_slugs.append(target)

        if not changed:
            continue

        new_inner = "\n" + "".join(f'  "{s}",\n' for s in new_slugs)
        new_fm = fm[: langs_match.start(2)] + new_inner + fm[langs_match.end(2) :]
        new_text = text[: fm_match.start(2)] + new_fm + text[fm_match.end(2) :]
        task_path.write_text(new_text)
        files_changed += 1

    print(f"Files changed: {files_changed}\n")
    print(f"=== Remaps ({sum(remap_stats.values())} occurrences, {len(remap_stats)} distinct) ===")
    for k, v in remap_stats.most_common():
        print(f"  {v:4d}  {k}")
    print(f"\n=== Removals ({sum(remove_stats.values())} occurrences, {len(remove_stats)} distinct) ===")
    for k, v in remove_stats.most_common():
        print(f"  {v:4d}  {k}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
