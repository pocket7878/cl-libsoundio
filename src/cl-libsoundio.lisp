(in-package :cl-user)
(defpackage cl-libsoundio
  (:use :cl :cffi))

(in-package :cl-libsoundio)

(define-foreign-library libsoundio
    (:darwin (:or "libsoundio.dylib" "libsoundio.1.dylib"))
  (t (:default "libsoundio")))

(use-foreign-library libsoundio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SoundIO ENUM
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIoError
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcenum soundio-error
          :error-none
          :error-no-mem
          :error-init-audio-backend
          :error-system-resources
          :error-opening-device
          :error-no-such-device
          :error-invalid
          :error-backend-unavailable
          :error-streaming
          :error-incompatible-device
          :error-backend-disconnected
          :error-interrupted
          :error-underflow
          :error-encoding-string
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIoChannelID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcenum soundio-channel-id
  :channel-id-invalid
  :channel-id-front-left
  :channel-id-front-right
  :channel-id-front-center
  :channel-id-lfe
  :channel-id-back-left
  :channel-id-back-right
  :channel-id-front-left-center
  :channel-id-front-right-center
  :channel-id-back-center
  :channel-id-side-left
  :channel-id-side-right
  :channel-id-top-center
  :channel-id-top-front-left
  :channel-id-top-front-center
  :channel-id-top-front-right
  :channel-id-top-back-left
  :channel-id-top-back-center
  :channel-id-top-back-right
  :channel-id-back-left-center
  :channel-id-back-right-center
  :channel-id-front-left-wide
  :channel-id-front-right-wide
  :channel-id-front-left-high
  :channel-id-front-center-high
  :channel-id-front-right-high
  :channel-id-top-front-left-center
  :channel-id-top-front-right-center
  :channel-id-top-side-left
  :channel-id-top-side-right
  :channel-id-left-lfe
  :channel-id-right-lfe
  :channel-id-lfe2
  :channel-id-bottom-center
  :channel-id-bottom-left-center
  :channel-id-bottom-right-center
  :channel-id-ms-mid
  :channel-id-ms-side
  :channel-id-ambisonic-w
  :channel-id-ambisonic-x
  :channel-id-ambisonic-y
  :channel-id-ambisonic-z
  :channel-id-xy-x
  :channel-id-xy-y
  :channel-id-headphones-left
  :channel-id-headphones-right
  :channel-id-click-track
  :channel-id-foreign-language
  :channel-id-hearing-impaired
  :channel-id-narration
  :channel-id-haptic
  :channel-id-dialog-centric-mix
  :channel-id-aux
  :channel-id-aux0
  :channel-id-aux1
  :channel-id-aux2
  :channel-id-aux3
  :channel-id-aux4
  :channel-id-aux5
  :channel-id-aux6
  :channel-id-aux7
  :channel-id-aux8
  :channel-id-aux9
  :channel-id-aux10
  :channel-id-aux11
  :channel-id-aux12
  :channel-id-aux13
  :channel-id-aux14
  :channel-id-aux15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIoBackend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcenum soundio-channel-layout-id
          :channel-layout-id-mono
          :channel-layout-id-stereo
          :channel-layout-id2-point1
          :channel-layout-id3-point0
          :channel-layout-id3-point0-back
          :channel-layout-id3-point1
          :channel-layout-id4-point0
          :channel-layout-id-quad
          :channel-layout-id-quad-side
          :channel-layout-id4-point1
          :channel-layout-id5-point0-back
          :channel-layout-id5-point0-side
          :channel-layout-id5-point1
          :channel-layout-id5-point1-back
          :channel-layout-id6-point0-side
          :channel-layout-id6-point0-front
          :channel-layout-id-hexagonal
          :channel-layout-id6-point1
          :channel-layout-id6-point1-back
          :channel-layout-id6-point1-front
          :channel-layout-id7-point0
          :channel-layout-id7-point0-front
          :channel-layout-id7-point1
          :channel-layout-id7-point1-wide
          :channel-layout-id7-point1-wide-back
          :channel-layout-id-octagonal)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIODeviceAim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcenum soundio-device-aim
    :aim-input
  :aim-output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIoFormat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcenum soundio-format
    :format-invalid
  :format-s8
  :format-u8
  :format-s16-le
  :format-s16-be
  :format-u16-le
  :format-u16-be
  :format-s24-le
  :format-s24-be
  :format-u24-le
  :format-u24-be
  :format-s32-le
  :format-s32-be
  :format-u32-le
  :format-u32-be
  :format-float32-le
  :format-float32-be
  :format-float64-le
  :format-float64-be
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SoundIO Struct
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIOChannelLayout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcstruct soundio-channel-layout
            (name :string)
            (channel-count :int)
            (channels soundio-channel-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIoSimpleRateRange
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcstruct soundio-sample-rate-range
            (min :int)
            (max :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIoChannelArea
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcstruct soundio-channel-area
            (ptr :string)
            (step :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcstruct soundio
            (userdata :pointer)
            (on-device-change :pointer)
            (on-bakend-disconnect :pointer)
            (on-events-signal :pointer)
            (current-backend soundio-backend)
            (app-name :string)
            (emit-rtprio-warning :pointer)
            (jack-info-callback :pointer)
            (jack-error-callback :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIODevice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcstruct soundio-device
            (soundio :pointer)
            (id :string)
            (name :string)
            (aim soundio-device-aim)
            (layout :pointer)
            (layout-count :int)
            (current-layout soundio-channel-layout)
            (format-count :int)
            (current-format soundio-format)
            (sample-rates :pointer)
            (sample-rate-count :int)
            (sample-rate-current :int)
            (software-latency-min :double)
            (software-latency-max :double)
            (software-latency-curent :double)
            (is-raw :boolean)
            (ref-count :int)
            (probe-error :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIoOutStream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcstruct soundio-outstream
            (device :pointer)
            (format soundio-format)
            (sample-rate :int)
            (layout soundio-channel-layout)
            (software-latency :double)
            (user-data :pointer)
            (write-callback :pointer)
            (underflow-callback :pointer)
            (error-callback :pointer)
            (name :string)
            (non-terminal-hint :boolean)
            (bytes-per-frame :int)
            (bytes-per-sample :int)
            (layout-error :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SoundIoInStream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcstruct soundio-instream
            (device :pointer)
            (format soundio-format)
            (sample-rate :int)
            (layout soundio-channel-layout)
            (software-latency :double)
            (user-data :pointer)
            (read-callback :pointer)
            (overflow-callback :pointer)
            (error-callback :pointer)
            (name :string)
            (non-terminal-hint :boolean)
            (bytes-per-frame :int)
            (bytes-per-sample :int)
            (layout-error :int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SoundIO Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defcfun "soundio_get_channel_name" :string (id soundio-channel-id))
(defcfun "soundio_parse_channel_name" soundio-channel-id 
         (str :string)
         (str-len :int))
(defcfun "soundio_channel_layout_builtin_count" :int)
(defcfun "soundio_channel_layout_get_builtin" :pointer
         (index :int))
(defcfun "soundio_channel_layout_get_default" :pointer
         (channel-count :int))
(defcfun "soundio_channel_layout_find_channel" :int
         (layout :pointer)
         (channel soundio-channel-id))
(defcfun "soundio_channel_layout_detect_builtin" :boolean
         (layout :pointer))
(defcfun "soundio_best_matching_channel_layout" :pointer
         (preferred-layouts :pointer)
         (preferred-layout-count :int)
         (available-layouts :pointer)
         (available-layout-count :int))
(defcfun "soundio_sort_channel_layouts" :void
         (layouts :pointer)
         (layout-count :int))
(defcfun "soundio_get_bytes_per_sample" :int
         (format soundio-format))
(defcfun "soundio_get_bytes_per_second" :int
         (format soundio-format)
         (channel-count :int)
         (sample-rate :int))
(defcfun "soundio_format_string" :string
         (format soundio-format))
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
(defcfun "soundio_device_supports_format" :boolean
         (device :pointer)
         (format soundio-format))
(defcfun "soundio_device_supports_layout" :boolean
         (device :pointer)
         (layout :pointer))
(defcfun "soundio_device_supports_sample_rate" :boolean
         (device :pointer)
         (sample-rate :int))
(defcfun "soundio_device_nearest_sample_rate" :int
         (device :pointer)
         (sample-rate :int))
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
