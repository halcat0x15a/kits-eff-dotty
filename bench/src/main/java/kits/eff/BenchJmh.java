package kits.eff;

import org.openjdk.jmh.annotations.Benchmark;

public class BenchJmh {
  @Benchmark
  public void benchEffJmh() {
      Bench bench = new Bench();
      bench.benchEffCall();
  }

  @Benchmark
  public void benchTransJmh() {
      Bench bench = new Bench();
      bench.benchTransCall();
  }
}
