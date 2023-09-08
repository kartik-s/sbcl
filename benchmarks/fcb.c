#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#ifdef _WIN32
#include <process.h>
#include <Windows.h>
#else
#include <pthread.h>
#include <time.h>
#endif

/* Generic benchmarking routine */
struct timed_run_args {
  void (*body_func)(void *);
  void *body_arg;
};

#ifdef _WIN32
__stdcall unsigned int timed_run(LPVOID arg)
#else
  void *timed_run(void *arg)
#endif
{
#ifdef _WIN32
  LARGE_INTEGER start_ticks, end_ticks, ticks_per_sec;
  QueryPerformanceFrequency(&ticks_per_sec);
  QueryPerformanceCounter(&start_ticks);
#else
  struct timespec start, end;
  clock_gettime(CLOCK_MONOTONIC, &start);
#endif

  struct timed_run_args *args = arg;
  args->body_func(args->body_arg);

  double elapsed_time;
#ifdef _WIN32
  QueryPerformanceCounter(&end_ticks);
  elapsed_time = ((double) (end_ticks.QuadPart - start_ticks.QuadPart)) / ticks_per_sec.QuadPart;
#else
  clock_gettime(CLOCK_MONOTONIC, &end);
  elapsed_time = (end.tv_nsec - start.tv_nsec) / 1e9 + (end.tv_sec - start.tv_sec);
#endif
  printf("elapsed time: %f seconds\n", elapsed_time);

  return 0;
}

/* Benchmark bodies */
struct body_args {
  uint64_t (*func)(uint64_t);
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

void control_body(void *arg)
{
  struct body_args *args = arg;
  uint64_t sum = 0;

  for (uint64_t i = 0; i < args->n_calls; i++) {
    uint64_t x = i % args->arg_mod;
    sum = (sum + x*x) % args->sum_mod;
  }

  assert(sum == expected_value(args->n_calls, args->arg_mod, args->sum_mod));
}

void call_body(void *arg)
{
  struct body_args *args = arg;
  uint64_t sum = 0;

  for (uint64_t i = 0; i < args->n_calls; i++) {
    sum = (sum + args->func(i % args->arg_mod)) % args->sum_mod;
  }

  assert(sum == expected_value(args->n_calls, args->arg_mod, args->sum_mod));
}

/* Exported benchmarking routines for Lisp */
#ifdef _WIN32
__declspec(dllexport)
#endif
uint64_t square(uint64_t x)
{
  return x * x;
}

#ifdef _WIN32
__declspec(dllexport)
#endif
void run_benchmark(void *fn_ptr, uint64_t n_calls, uint64_t arg_mod, uint64_t sum_mod, int from_new_thread_p)
{
  struct timed_run_args run_arg;
  struct body_args body_arg;

  body_arg.func = fn_ptr;
  body_arg.n_calls = n_calls;
  body_arg.arg_mod = arg_mod;
  body_arg.sum_mod = sum_mod;

  run_arg.body_func = fn_ptr ? call_body : control_body;
  run_arg.body_arg = &body_arg;

  if (from_new_thread_p) {
#if _WIN32
    HANDLE t = (HANDLE) _beginthreadex(NULL, 0, timed_run, &run_arg, 0, NULL);
    WaitForSingleObject(t, INFINITE);
#else
    pthread_t t;
    pthread_create(&t, NULL, timed_run, &run_arg);
    pthread_join(t, NULL);
#endif
  } else {
    timed_run(&run_arg);
  }
}
