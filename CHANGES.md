# 0.4.0

* Use conditional compilation to avoid using NOTE_OOB, NOTE_SIGNAL on FreeBSD (#18, @DavidAlphaFox)
* Disable EVFILT_EXCEPT on OpenBSD (#18, @DavidAlphaFox)

# 0.3.0

* Support EVFILT_USER

# 0.2.0

* Remove the use of ctypes
* Limit support to 64 bit systems
* Add pre-defined constants for filter flags

# 0.1.0

* Initial version of kqueue-ml
