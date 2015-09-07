(in-package :cl-user)
(defpackage cl-libsoundio
  (:use :cl :cffi))

(in-package :cl-libsoundio)

(define-foreign-library libsoundio
    (:darwin (:or "libsoundio.dylib" "libsoundio.1.dylib"))
  (t (:default "libsoundio")))

(use-foreign-library libsoundio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIoBackend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcenum soundio-backend
    :backend-none
  :backend-jack
  :backend-pulse-audio
  :backend-alsa
  :backend-core-audio
  :backend-wasapi
  :backend-dummy)

(defcfun "soundio_create" :pointer)
(defcfun "soundio_destroy" :void (soundio :pointer))
(defcfun "soundio_connect" :int (soundio :pointer))
(defcfun "soundio_connect_backend" :int
  (soundio :pointer)
  (backend soundio-backend))
(defcfun "soundio_disconnect" :void (soundio :pointer))
(defcfun "soundio_strerror" :string (err :int))
(defcfun "soundio_backend_name" :string (backend soundio-backend))
(defcfun "soundio_backend_count" :int (soundio :pointer))
(defcfun "soundio_get_backend" soundio-backend (soundio :pointer) (index :int))
(defcfun "soundio_have_backend" :boolean (backend soundio-backend))
(defcfun "soundio_flush_events" :void (soundio :pointer))
(defcfun "soundio_wait_events" :void (soundio :pointer))
(defcfun "soundio_wakeup" :void (soundio :pointer))
(defcfun "soundio_force_device_scan" :void (soundio :pointer))
(defcfun "soundio_channel_layout_equal" :boolean
  (layout-a :pointer)
  (layout-b :pointer))

(defcfun "soundio_input_device_count" :int (soundio :pointer))
(defcfun "soundio_output_device_count" :int (soundio :pointer))
(defcfun "soundio_get_input_device" :pointer
  (soundio :pointer)
  (index :int))
(defcfun "soundio_get_output_device" :pointer
  (soundio :pointer)
  (index :int))
(defcfun "soundio_default_input_device_index" :int (soundio :pointer))
(defcfun "soundio_default_output_device_index" :int (soundio :pointer))
(defcfun "soundio_device_ref" :void (device :pointer))
(defcfun "soundio_device_unref" :void (device :pointer))
(defcfun "soundio_device_equal" :boolean (device-a :pointer) (device-b :pointer))
(defcfun "soundio_device_sort_channel_layouts" :void (device :pointer))

(defcfun "soundio_outstream_create" :pointer (device :pointer))
(defcfun "soundio_outstream_destroy" :void (outstream :pointer))
(defcfun "soundio_outstream_open" :int (outstream :pointer))
(defcfun "soundio_outstream_start" :int (outstream :pointer))
(defcfun "soundio_outstream_begin_write" :int
  (outstream :pointer)
  (areas :pointer)
  (frame-count :pointer))
(defcfun "soundio_outstream_end_write" :int
  (outstream :pointer))
(defcfun "soundio_outstream_clear_buffer" :int
  (outstream :pointer))
(defcfun "soundio_outstream_pause" :int
  (outstream :pointer)
  (pause :boolean))
(defcfun "soundio_outstream_get_latency" :int
  (outstream :pointer)
  (out_latency :pointer))
(defcfun "soundio_instream_create" :pointer
  (device :pointer))
(defcfun "soundio_instream_destroy" :void
  (instream :pointer))
(defcfun "soundio_instream_open" :int
  (instream :pointer))
(defcfun "soundio_instream_start" :int
  (instream :pointer))
(defcfun "soundio_instream_begin_read" :int
  (instream :pointer)
  (areas :pointer)
  (frame-count :pointer))
(defcfun "soundio_instream_end_read" :int
  (instream :pointer))
(defcfun "soundio_instream_pause" :int
  (instream :pointer)
  (pause :boolean))
(defcfun "soundio_instream_get_latency" :int
  (instream :pointer)
  (out-latency :pointer))
(defcfun "soundio_ring_buffer_create" :pointer
  (soundio :pointer)
  (requested-capacity :int))
(defcfun "soundio_ring_buffer_destroy" :void
  (ring-buffer :pointer))
(defcfun "soundio_ring_buffer_capacity" :int
  (ring-buffer :pointer))
(defcfun "soundio_ring_buffer_write_ptr" :string
  (ring-buffer :pointer))
(defcfun "soundio_ring_buffer_advance_write_ptr" :void
  (ring-buffer :pointer)
  (count :int))
(defcfun "soundio_ring_buffer_read_ptr" :string
  (ring-buffer :pointer))
(defcfun "soundio_ring_buffer_advance_read_ptr" :void
  (ring-buffer :pointer)
  (count :int))
(defcfun "soundio_ring_buffer_fill_count" :int
  (ring-buffer :pointer))
(defcfun "soundio_ring_buffer_free_count" :int
  (ring-buffer :pointer))
(defcfun "soundio_ring_buffer_clear" :void
  (ring-buffer :pointer))




