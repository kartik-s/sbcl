#include <assert.h>
#include <process.h>
#include <stdint.h>
#include <stdio.h>
#include <Windows.h>

struct benchmark_args {
  void *fn_ptr;
  int n_calls;
  int arg_mod;
  int sum_mod;
};

uint64_t sum_of_squares(uint64_t n)
{
  return n*(n+1)*(2*n+1)/6;
}

uint64_t expected_value(uint64_t n_calls, uint64_t arg_mod, uint64_t sum_mod)
{
  uint64_t full_sum = ((n_calls / arg_mod) * sum_of_squares(arg_mod - 1)) % sum_mod;
  uint64_t partial_sum = sum_of_squares((n_calls % arg_mod) - 1) % sum_mod;

  return (full_sum + partial_sum) % sum_mod;
}

__stdcall
unsigned int run_call_benchmark(void *argp)
{
  struct benchmark_args *args = argp;
  uint64_t sum = 0;
  uint64_t (*fn)(uint64_t) = args->fn_ptr;

  LARGE_INTEGER start_ticks, end_ticks, ticks_per_sec;
  double elapsed_time;

  QueryPerformanceFrequency(&ticks_per_sec);
  QueryPerformanceCounter(&start_ticks);

  for (uint64_t i = 0; i < args->n_calls; i++) {
    sum = (sum + fn(i % args->arg_mod)) % args->sum_mod;
  }

  QueryPerformanceCounter(&end_ticks);

  assert(sum == expected_value(args->n_calls, args->arg_mod, args->sum_mod));

  elapsed_time = ((double) (end_ticks.QuadPart - start_ticks.QuadPart)) / ticks_per_sec.QuadPart;
  printf("elapsed time: %f seconds\n", elapsed_time);

  return 0;
}

__declspec(dllexport)
void benchmark_control(uint64_t n_calls, uint64_t arg_mod, uint64_t sum_mod)
{
  uint64_t sum = 0;

  LARGE_INTEGER start_ticks, end_ticks, ticks_per_sec;
  double elapsed_time;

  QueryPerformanceFrequency(&ticks_per_sec);
  QueryPerformanceCounter(&start_ticks);

  for (uint64_t i = 0; i < n_calls; i++) {
    uint64_t x = i % arg_mod;
    sum = (sum + x*x) % sum_mod;
  }

  QueryPerformanceCounter(&end_ticks);

  assert(sum == expected_value(n_calls, arg_mod, sum_mod));

  elapsed_time = ((double)(end_ticks.QuadPart - start_ticks.QuadPart)) / ticks_per_sec.QuadPart;
  printf("elapsed time: %f seconds\n", elapsed_time);
}

__declspec(dllexport)
uint64_t square(uint64_t x)
{
  return x * x;
}

__declspec(dllexport)
void benchmark_calls_from_same_thread(void *fn_ptr, uint64_t n_calls, uint64_t arg_mod, uint64_t sum_mod)
{
  struct benchmark_args args;

  args.fn_ptr = fn_ptr;
  args.n_calls = n_calls;
  args.arg_mod = arg_mod;
  args.sum_mod = sum_mod;

  run_call_benchmark(&args);
}

__declspec(dllexport)
__stdcall void benchmark_calls_from_new_thread(void *fn_ptr, uint64_t n_calls, uint64_t arg_mod, uint64_t sum_mod)
{
  struct benchmark_args args;

  args.fn_ptr = fn_ptr;
  args.n_calls = n_calls;
  args.arg_mod = arg_mod;
  args.sum_mod = sum_mod;

  HANDLE t = (HANDLE) _beginthreadex(NULL, 0, run_call_benchmark, &args, 0, NULL);

  WaitForSingleObject(t, INFINITE);
}
