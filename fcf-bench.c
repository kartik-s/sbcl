#include <assert.h>
#include <stdio.h>
#include <Windows.h>

struct bench_args {
  void *fn_ptr;
  int n_calls;
};

__stdcall void run_benchmark(void *fn_ptr, int n_calls)
{
  int sum = 0;
  int (*fn)(int) = fn_ptr;
  LARGE_INTEGER start_ticks, end_ticks, ticks_per_sec;
  double elapsed_time;

  QueryPerformanceFrequency(&ticks_per_sec);
  QueryPerformanceCounter(&start_ticks);
  
  for (int i = 0; i < n_calls; i++) {
    sum += fn(3);
  }

  QueryPerformanceCounter(&end_ticks);
  elapsed_time = ((double) (end_ticks.QuadPart - start_ticks.QuadPart)) / ticks_per_sec.QuadPart;
  printf("elapsed time: %f seconds\n", elapsed_time);

  assert(sum == (9 * n_calls));
}

__declspec(dllexport)
__stdcall void benchmark_regular(void *fn_ptr, int n_calls)
{
  struct bench_args args;

  args.fn_ptr = fn_ptr;
  args.n_calls = n_calls;
  
  run_benchmark(&args);
}

__declspec(dllexport)
__stdcall void benchmark_callback_fiber(void *fn_ptr, int n_calls)
{
  struct bench_args args;

  args.fn_ptr = fn_ptr;
  args.n_calls = n_calls;
  
  HANDLE t = _beginthreadex(NULL, 0, benchmark_regular, &args, 0, NULL);

  WaitForSingleObject(t, INFINITE);
}
