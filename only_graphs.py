def plot(filename, title, xlabel, ylabel, xticks, yticks):
    _, ax = plt.subplots(figsize=(5, 3))
    ax.set_title(title)
    if xlabel != "":
        ax.set_xlabel(xlabel)
    if ylabel != "":
        ax.set_ylabel(ylabel)
    yticks, xticks = zip(*sorted(zip(yticks, xticks), key=lambda x: x[1]))
    y_pos = np.arange(len(yticks))
    bar_height = 0.6
    ax.barh(y_pos, xticks, bar_height, color="#1a85ff", left=0, align="center")
    ax.set_yticks(y_pos)
    ax.set_yticklabels(yticks)
    for i, v in enumerate(xticks):
        ax.text(v, i, f"{v:.2f}x", va="center")
    ax.set_xlim(0, max(xticks) * 1.2)
    ax.set_ylim(-0.5, len(yticks) - 0.5)
    ax.axvline(1, color="black", linestyle="dotted", linewidth=1)
    ax.set_axisbelow(True)
    ax.grid(axis="x", alpha=0.7)
    plt.tight_layout()
    plt.savefig(f"out/{filename}.png")

if __name__ == "__main__":
    import os, json
    import subprocess as sp
    import matplotlib.pyplot as plt
    import numpy as np
    benches = [
        "bench_calc", "bench_calc_lss", 
        "bench_parse_json", "bench_parse_json_lss", 
        "bench_primes_sieve", "bench_primes_sieve_lss",
        "bench_primes_iter", "bench_primes_iter_lss",
        "bench_quicksort", "bench_quicksort_lss",
        "bench_unify", "bench_unify_lss",
        "bench_minhs", "bench_minhs_lss",
    ]
    with open("out/processed.json") as f:
        out_processed = json.load(f)
    bench_names = [bench.replace("bench_", "") for bench in benches if not bench.endswith("_lss")]
    speedups = [out_processed[bench]["speedup"] for bench in benches if bench.endswith("_lss")]
    bin_sizes = {bench: os.path.getsize(f"bin/{bench}") for bench in benches}
    rel_bin_sizes = []
    for bench in benches:
        if not bench.endswith("_lss"):
            continue
        rel_bin_sizes.append(bin_sizes[bench] / bin_sizes[bench.replace("_lss", "")])
    plot("speedup_chart", "Speedup due to Lambda Set Specialization", "Speedup Factor", "", speedups, bench_names)
    plot("bin_size_chart", "Binary Sizes after Lambda Set Specialization", "Size Ratio (Lower is Better)", "", rel_bin_sizes, bench_names)