RUNS = 100

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
    ax.set_title("Relative Performance (Lower is Better)")
    ax.set_ylabel("Time Relative to Regular Version")
    pairs = []
    for bench in benches:
        if not bench.endswith("_lss"):
            pairs.append((bench, bench + "_lss"))
    data = []
    labels = []
    positions = []
    colors = ['#1a85ff', '#34a853']
    for idx, (regular, lss) in enumerate(pairs):
        regular_median = np.median(out_raw[regular])
        normalized_regular = [t / regular_median for t in out_raw[regular]]
        normalized_lss = [t / regular_median for t in out_raw[lss]]
        data.append(normalized_regular)
        data.append(normalized_lss)
        labels.append(regular.replace("bench_", ""))
        labels.append(lss.replace("bench_", ""))
        positions.append(idx * 3)
        positions.append(idx * 3 + 1)
    bp = ax.boxplot(data, positions=positions, patch_artist=True, widths=0.6)
    for i, box in enumerate(bp['boxes']):
        box.set(facecolor=colors[i % 2])
    ax.set_xticks([p + 0.5 for p in positions[::2]])
    ax.set_xticklabels([labels[i].replace("_lss", "") for i in range(0, len(labels), 2)], rotation=45)
    ax.axhline(1.0, color='black', linestyle='dotted', alpha=0.7)
    from matplotlib.patches import Patch
    legend_elements = [
        Patch(facecolor=colors[0], label='Regular'),
        Patch(facecolor=colors[1], label='LSS')
    ]
    ax.legend(handles=legend_elements, loc='upper right')
    ax.grid(axis="y", alpha=0.7)
    plt.tight_layout()
    plt.savefig("out/normalized_benchmark_boxplot.png")

if __name__ == "__main__":
    import glob, os, time, json
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
    iterations = {
        "bench_calc": 100,
        "bench_parse_json": 10,
        "bench_primes_sieve": 100,
        "bench_primes_iter": 100,
        "bench_quicksort": 10,
        "bench_unify": 10,
        "bench_minhs": 10000000,
    }
    if len(glob.glob(".ghc.environment.*")) == 0:
        sp.run(["cabal", "install", "--lib", "--package-env", ".", "persistent-vector", "Cabal-3.6.3.0"])
    os.makedirs("bin", exist_ok=True)
    os.makedirs("out", exist_ok=True)
    [sp.run(["ghc", f"src/{bench}.hs", "-o", f"bin/{bench}"]) for bench in benches]
    input = {}
    for bench in benches:
        name = bench.replace("_lss", "")
        input[bench] = [f"{iterations[name]}"]
        with open(f"in/{bench.replace('_lss', '')}.txt") as f:
            while line := f.readline():
                input[bench].append(f"{line.rstrip()}")
        input[bench].append("\n")
    out_raw = {}
    out_processed = {}
    for bench in benches:
        out_raw[bench] = []
        for i in range(RUNS):
            inp = "\n".join(input[bench])
            s = time.time()
            res = sp.run(["bin/" + bench], input=inp, text=True, stdout=sp.PIPE, stderr=sp.PIPE, check=False)
            e = time.time()
            t = e - s
            if t < 0:
                i -= 1
                continue
            print(f"{bench} run {i + 1} took {e - s:.6f} seconds")
            out_raw[bench].append(t)
        out_processed[bench] = {}
        out_processed[bench]["min"] = min(out_raw[bench])
        print(f"{bench} min: {out_processed[bench]['min']:.6f}")
        out_processed[bench]["max"] = max(out_raw[bench])
        print(f"{bench} max: {out_processed[bench]['max']:.6f}")
        out_processed[bench]["mean"] = sum([float(x) for x in out_raw[bench]]) / RUNS
        print(f"{bench} mean: {out_processed[bench]['mean']:.6f}")
        out_processed[bench]["median"] = sorted([float(x) for x in out_raw[bench]])[RUNS // 2]
        print(f"{bench} median: {out_processed[bench]['median']:.6f}")
        out_processed[bench]["stddev"] = (sum([(float(x) - out_processed[bench]["mean"]) ** 2 for x in out_raw[bench]]) / RUNS) ** 0.5
        print(f"{bench} stddev: {out_processed[bench]['stddev']:.6f}")
    for bench in benches:
        if not bench.endswith("_lss"):
            out_processed[bench]["min_speedup"] = 1
            out_processed[bench]["mean_speedup"] = 1
            out_processed[bench]["median_speedup"] = 1
        else:
            out_processed[bench]["min_speedup"] = out_processed[bench.replace("_lss", "")]["min"] / out_processed[bench]["min"]
            out_processed[bench]["mean_speedup"] = out_processed[bench.replace("_lss", "")]["mean"] / out_processed[bench]["mean"]
            out_processed[bench]["median_speedup"] = out_processed[bench.replace("_lss", "")]["median"] / out_processed[bench]["median"]
    with open("out/raw.json", "w") as f:
        f.write(json.dumps(out_raw, indent=4, sort_keys=True, separators=(",", ": ")))
    with open("out/processed.json", "w") as f:
        f.write(json.dumps(out_processed, indent=4, sort_keys=True, separators=(",", ": ")))
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
    plot_boxplot(out_raw, benches)