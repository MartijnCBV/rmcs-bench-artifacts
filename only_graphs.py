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

def plot_boxplot(out_raw, benches):
    _, ax = plt.subplots(figsize=(12, 8))
    ax.set_title("Performance Distribution (Lower is Better)")
    ax.set_ylabel("Time (seconds)")
    pairs = []
    for bench in benches:
        if not bench.endswith("_lss"):
            pairs.append((bench, bench + "_lss"))
    data = []
    labels = []
    positions = []
    colors = ['#1a85ff', '#34a853']
    for idx, (regular, lss) in enumerate(pairs):
        data.append(out_raw[regular])
        data.append(out_raw[lss])
        labels.append(regular.replace("bench_", ""))
        labels.append(lss.replace("bench_", ""))
        positions.append(idx * 3)     # Position for regular benchmark
        positions.append(idx * 3 + 1) # Position for LSS variant
    bp = ax.boxplot(data, positions=positions, patch_artist=True, widths=0.6)
    for i, box in enumerate(bp['boxes']):
        box.set(facecolor=colors[i % 2])
    ax.set_xticks([p + 0.5 for p in positions[::2]])
    ax.set_xticklabels([labels[i].replace("_lss", "") for i in range(0, len(labels), 2)], rotation=45)
    from matplotlib.patches import Patch
    legend_elements = [
        Patch(facecolor=colors[0], label='Regular'),
        Patch(facecolor=colors[1], label='LSS')
    ]
    ax.legend(handles=legend_elements, loc='upper right')
    ax.grid(axis="y", alpha=0.7)
    plt.tight_layout()
    plt.savefig("out/benchmark_boxplot.png")

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
    min_speedups = [out_processed[bench]["min_speedup"] for bench in benches if bench.endswith("_lss")]
    mean_speedups = [out_processed[bench]["mean_speedup"] for bench in benches if bench.endswith("_lss")]
    median_speedups = [out_processed[bench]["median_speedup"] for bench in benches if bench.endswith("_lss")]
    bin_sizes = {bench: os.path.getsize(f"bin/{bench}") for bench in benches}
    rel_bin_sizes = []
    for bench in benches:
        if not bench.endswith("_lss"):
            continue
        rel_bin_sizes.append(bin_sizes[bench] / bin_sizes[bench.replace("_lss", "")])
    plot("min_speedup_chart", "Min Speedup due to Lambda Set Specialization", "Speedup Factor", "", min_speedups, bench_names)
    plot("mean_speedup_chart", "Mean Speedup due to Lambda Set Specialization", "Speedup Factor", "", mean_speedups, bench_names)
    plot("median_speedup_chart", "Median Speedup due to Lambda Set Specialization", "Speedup Factor", "", median_speedups, bench_names)
    plot("bin_size_chart", "Binary Sizes after Lambda Set Specialization", "Size Ratio (Lower is Better)", "", rel_bin_sizes, bench_names)
    with open("out/raw.json") as f:
        out_raw = json.load(f)
    plot_boxplot(out_raw, benches)