use criterion::{black_box, criterion_group, criterion_main, Criterion};
use problem12::{count_divisors, count_divisors_optimized, triangle_number};

fn benchmark_count_divisors(c: &mut Criterion) {
    let mut group = c.benchmark_group("count_divisors");
    
    // Small number
    group.bench_function("count_divisors_small", |b| {
        b.iter(|| count_divisors(black_box(28)))
    });
    
    // Medium number
    group.bench_function("count_divisors_medium", |b| {
        b.iter(|| count_divisors(black_box(1000)))
    });
    
    // Large number (Project Euler answer)
    group.bench_function("count_divisors_large", |b| {
        b.iter(|| count_divisors(black_box(76576500)))
    });

    // Optimized versions
    group.bench_function("count_divisors_optimized_small", |b| {
        b.iter(|| count_divisors_optimized(black_box(28)))
    });

    group.bench_function("count_divisors_optimized_medium", |b| {
        b.iter(|| count_divisors_optimized(black_box(1000)))
    });

    group.bench_function("count_divisors_optimized_large", |b| {
        b.iter(|| count_divisors_optimized(black_box(76576500)))
    });

    group.finish();
}

fn benchmark_triangle_number(c: &mut Criterion) {
    let mut group = c.benchmark_group("triangle_number");
    
    group.bench_function("triangle_number_small", |b| {
        b.iter(|| triangle_number(black_box(7)))
    });
    
    group.bench_function("triangle_number_large", |b| {
        b.iter(|| triangle_number(black_box(12375)))
    });
    
    group.finish();
}

criterion_group!(benches, benchmark_count_divisors, benchmark_triangle_number);
criterion_main!(benches);
