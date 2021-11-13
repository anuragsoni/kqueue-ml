#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/event.h>
#include <sys/time.h>
#include <sys/types.h>

#define Kqueue_constant(name, i)                                               \
  CAMLprim value name(value unit) { return Val_int(i); }

CAMLprim value kqueue_ml_kqueue_create(value unit) {
  int k;
  k = kqueue();
  if (k == -1)
    uerror("kqueue", Nothing);
  fcntl(k, F_SETFD, FD_CLOEXEC);
  return Val_long(k);
}

CAMLprim value kqueue_ml_kevent_sizeof(value unit) {
  return Val_long(sizeof(struct kevent));
}

CAMLprim value kqueue_ml_kevent_offset_event_fd(value unit) {
  return Val_int(offsetof(struct kevent, ident));
}

CAMLprim value kqueue_ml_kevent_offset_filter(value unit) {
  return Val_int(offsetof(struct kevent, filter));
}

CAMLprim value kqueue_ml_kevent_offset_flags(value unit) {
  return Val_int(offsetof(struct kevent, flags));
}

CAMLprim value kqueue_ml_modify_fd(value kqueue_fd, value fd, value filter, value flags) {
  struct kevent event;
  EV_SET(&event, Long_val(fd), Int_val(filter), Int_val(flags), 0, 0, 0);

  int ret;
  ret = kevent(Long_val(kqueue_fd), &event, 1, NULL, 0, NULL);
  if (ret == -1)
    uerror("kevent", Nothing);
  return Val_long(ret);
}

CAMLprim value kqueue_ml_wait(value kqueue_fd, value eventlist, value timeout) {
  CAMLparam3(kqueue_fd, eventlist, timeout);
  struct kevent * evs;
  int ret, event_count;
  evs = (struct kevent *) Caml_ba_data_val(eventlist);
  event_count = Caml_ba_array_val(eventlist)->dim[0] / sizeof (struct kevent);
  if (timeout == 0) {
    struct timespec t = { 0, 0 };
    ret = kevent(Long_val(kqueue_fd), NULL, 0, evs, event_count, &t);
  } else {
    struct timespec t;
    long ms = Long_val(timeout);
    t.tv_sec = ms / 1000;
    t.tv_nsec = (ms % 1000) * 1000000;
    caml_enter_blocking_section();
    ret = kevent(Long_val(kqueue_fd), NULL, 0, evs, event_count, &t);
    caml_leave_blocking_section();
  }
  if (ret == -1)
    uerror("kevent", Nothing);
  CAMLreturn(Val_long(ret));
}

Kqueue_constant(kqueue_filter_evfilt_read, EVFILT_READ)
Kqueue_constant(kqueue_filter_evfilt_write, EVFILT_WRITE)

Kqueue_constant(kqueue_flag_ev_add, EV_ADD)
Kqueue_constant(kqueue_flag_ev_enable, EV_ENABLE)
Kqueue_constant(kqueue_flag_ev_disable, EV_DISABLE)
Kqueue_constant(kqueue_flag_ev_delete, EV_DELETE)
Kqueue_constant(kqueue_flag_ev_oneshot, EV_ONESHOT)
Kqueue_constant(kqueue_flag_ev_clear, EV_CLEAR)
Kqueue_constant(kqueue_flag_ev_eof, EV_EOF)
Kqueue_constant(kqueue_flag_ev_error, EV_ERROR)
