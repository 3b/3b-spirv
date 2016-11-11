;;; generated from files with following copyright:

;;; Copyright (c) 2014-2016 The Khronos Group Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and/or associated documentation files (the "Materials"),
;;; to deal in the Materials without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Materials, and to permit persons to whom the
;;; Materials are furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Materials.
;;;
;;; MODIFICATIONS TO THIS FILE MAY MEAN IT NO LONGER ACCURATELY REFLECTS KHRONOS
;;; STANDARDS. THE UNMODIFIED, NORMATIVE VERSIONS OF KHRONOS SPECIFICATIONS AND
;;; HEADER INFORMATION ARE LOCATED AT https://www.khronos.org/registry/
;;;
;;; THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM,OUT OF OR IN CONNECTION WITH THE MATERIALS OR THE USE OR OTHER DEALINGS
;;; IN THE MATERIALS.

(in-package #:3b-spirv)
(defparameter *spec*
  (list :magic 119734787
        :version 65792
        :revision 3
        :opcode-mask 65535
        :word-count-shift 16
        :enums
        (alexandria:plist-hash-table
         (list :access-qualifier
               (list :value
                     (alexandria:plist-hash-table
                      (list :read-only 0
                            :write-only 1
                            :read-write 2)))
               :addressing-model
               (list :value
                     (alexandria:plist-hash-table
                      (list :logical 0
                            :physical32 1
                            :physical64 2)))
               :built-in
               (list :value
                     (alexandria:plist-hash-table
                      (list :position 0
                            :point-size 1
                            :clip-distance 3
                            :cull-distance 4
                            :vertex-id 5
                            :instance-id 6
                            :primitive-id 7
                            :invocation-id 8
                            :layer 9
                            :viewport-index 10
                            :tess-level-outer 11
                            :tess-level-inner 12
                            :tess-coord 13
                            :patch-vertices 14
                            :frag-coord 15
                            :point-coord 16
                            :front-facing 17
                            :sample-id 18
                            :sample-position 19
                            :sample-mask 20
                            :frag-depth 22
                            :helper-invocation 23
                            :num-workgroups 24
                            :workgroup-size 25
                            :workgroup-id 26
                            :local-invocation-id 27
                            :global-invocation-id 28
                            :local-invocation-index 29
                            :work-dim 30
                            :global-size 31
                            :enqueued-workgroup-size 32
                            :global-offset 33
                            :global-linear-id 34
                            :subgroup-size 36
                            :subgroup-max-size 37
                            :num-subgroups 38
                            :num-enqueued-subgroups 39
                            :subgroup-id 40
                            :subgroup-local-invocation-id 41
                            :vertex-index 42
                            :instance-index 43
                            :subgroup-eq-mask-khr 4416
                            :subgroup-ge-mask-khr 4417
                            :subgroup-gt-mask-khr 4418
                            :subgroup-le-mask-khr 4419
                            :subgroup-lt-mask-khr 4420)))
               :capability
               (list :value
                     (alexandria:plist-hash-table
                      (list :matrix 0
                            :shader 1
                            :geometry 2
                            :tessellation 3
                            :addresses 4
                            :linkage 5
                            :kernel 6
                            :vector16 7
                            :float16-buffer 8
                            :float16 9
                            :float64 10
                            :int64 11
                            :int64-atomics 12
                            :image-basic 13
                            :image-read-write 14
                            :image-mipmap 15
                            :pipes 17
                            :groups 18
                            :device-enqueue 19
                            :literal-sampler 20
                            :atomic-storage 21
                            :int16 22
                            :tessellation-point-size 23
                            :geometry-point-size 24
                            :image-gather-extended 25
                            :storage-image-multisample 27
                            :uniform-buffer-array-dynamic-indexing 28
                            :sampled-image-array-dynamic-indexing 29
                            :storage-buffer-array-dynamic-indexing 30
                            :storage-image-array-dynamic-indexing 31
                            :clip-distance 32
                            :cull-distance 33
                            :image-cube-array 34
                            :sample-rate-shading 35
                            :image-rect 36
                            :sampled-rect 37
                            :generic-pointer 38
                            :int8 39
                            :input-attachment 40
                            :sparse-residency 41
                            :min-lod 42
                            :sampled-1d 43
                            :image-1d 44
                            :sampled-cube-array 45
                            :sampled-buffer 46
                            :image-buffer 47
                            :image-ms-array 48
                            :storage-image-extended-formats 49
                            :image-query 50
                            :derivative-control 51
                            :interpolation-function 52
                            :transform-feedback 53
                            :geometry-streams 54
                            :storage-image-read-without-format 55
                            :storage-image-write-without-format 56
                            :multi-viewport 57
                            :subgroup-dispatch 58
                            :named-barrier 59
                            :pipe-storage 60
                            :subgroup-ballot-khr 4423)))
               :decoration
               (list :value
                     (alexandria:plist-hash-table
                      (list :relaxed-precision 0
                            :spec-id 1
                            :block 2
                            :buffer-block 3
                            :row-major 4
                            :col-major 5
                            :array-stride 6
                            :matrix-stride 7
                            :glsl-shared 8
                            :glsl-packed 9
                            :c-packed 10
                            :built-in 11
                            :no-perspective 13
                            :flat 14
                            :patch 15
                            :centroid 16
                            :sample 17
                            :invariant 18
                            :restrict 19
                            :aliased 20
                            :volatile 21
                            :constant 22
                            :coherent 23
                            :non-writable 24
                            :non-readable 25
                            :uniform 26
                            :saturated-conversion 28
                            :stream 29
                            :location 30
                            :component 31
                            :index 32
                            :binding 33
                            :descriptor-set 34
                            :offset 35
                            :xfb-buffer 36
                            :xfb-stride 37
                            :func-param-attr 38
                            :fp-rounding-mode 39
                            :fp-fast-math-mode 40
                            :linkage-attributes 41
                            :no-contraction 42
                            :input-attachment-index 43
                            :alignment 44
                            :max-byte-offset 45)))
               :dim
               (list :value
                     (alexandria:plist-hash-table
                      (list :dim-1d 0
                            :dim-2d 1
                            :dim-3d 2
                            :cube 3
                            :rect 4
                            :buffer 5
                            :subpass-data 6)))
               :execution-mode
               (list :value
                     (alexandria:plist-hash-table
                      (list :invocations 0
                            :spacing-equal 1
                            :spacing-fractional-even 2
                            :spacing-fractional-odd 3
                            :vertex-order-cw 4
                            :vertex-order-ccw 5
                            :pixel-center-integer 6
                            :origin-upper-left 7
                            :origin-lower-left 8
                            :early-fragment-tests 9
                            :point-mode 10
                            :xfb 11
                            :depth-replacing 12
                            :depth-greater 14
                            :depth-less 15
                            :depth-unchanged 16
                            :local-size 17
                            :local-size-hint 18
                            :input-points 19
                            :input-lines 20
                            :input-lines-adjacency 21
                            :triangles 22
                            :input-triangles-adjacency 23
                            :quads 24
                            :isolines 25
                            :output-vertices 26
                            :output-points 27
                            :output-line-strip 28
                            :output-triangle-strip 29
                            :vec-type-hint 30
                            :contraction-off 31
                            :initializer 33
                            :finalizer 34
                            :subgroup-size 35
                            :subgroups-per-workgroup 36)))
               :execution-model
               (list :value
                     (alexandria:plist-hash-table
                      (list :vertex 0
                            :tessellation-control 1
                            :tessellation-evaluation 2
                            :geometry 3
                            :fragment 4
                            :gl-compute 5
                            :kernel 6)))
               :fp-fast-math-mode
               (list :bit
                     (alexandria:plist-hash-table
                      (list :not-nan 0
                            :not-inf 1
                            :nsz 2
                            :allow-recip 3
                            :fast 4)))
               :fp-rounding-mode
               (list :value
                     (alexandria:plist-hash-table
                      (list :rte 0
                            :rtz 1
                            :rtp 2
                            :rtn 3)))
               :function-control
               (list :bit
                     (alexandria:plist-hash-table
                      (list :inline 0
                            :dont-inline 1
                            :pure 2
                            :const 3)))
               :function-parameter-attribute
               (list :value
                     (alexandria:plist-hash-table
                      (list :zext 0
                            :sext 1
                            :by-val 2
                            :sret 3
                            :no-alias 4
                            :no-capture 5
                            :no-write 6
                            :no-read-write 7)))
               :group-operation
               (list :value
                     (alexandria:plist-hash-table
                      (list :reduce 0
                            :inclusive-scan 1
                            :exclusive-scan 2)))
               :image-channel-data-type
               (list :value
                     (alexandria:plist-hash-table
                      (list :snorm-int8 0
                            :snorm-int16 1
                            :unorm-int8 2
                            :unorm-int16 3
                            :unorm-short565 4
                            :unorm-short555 5
                            :unorm-int101010 6
                            :signed-int8 7
                            :signed-int16 8
                            :signed-int32 9
                            :unsigned-int8 10
                            :unsigned-int16 11
                            :unsigned-int32 12
                            :half-float 13
                            :float 14
                            :unorm-int24 15
                            :unorm-int101010-2 16)))
               :image-channel-order
               (list :value
                     (alexandria:plist-hash-table
                      (list :r 0
                            :a 1
                            :rg 2
                            :ra 3
                            :rgb 4
                            :rgba 5
                            :bgra 6
                            :argb 7
                            :intensity 8
                            :luminance 9
                            :rx 10
                            :rgx 11
                            :rgbx 12
                            :depth 13
                            :depth-stencil 14
                            :srgb 15
                            :srgbx 16
                            :srgba 17
                            :sbgra 18
                            :abgr 19)))
               :image-format
               (list :value
                     (alexandria:plist-hash-table
                      (list :unknown 0
                            :rgba32f 1
                            :rgba16f 2
                            :r32f 3
                            :rgba8 4
                            :rgba8-snorm 5
                            :rg32f 6
                            :rg16f 7
                            :r11f-g11f-b10f 8
                            :r16f 9
                            :rgba16 10
                            :rgb10-a2 11
                            :rg16 12
                            :rg8 13
                            :r16 14
                            :r8 15
                            :rgba16-snorm 16
                            :rg16-snorm 17
                            :rg8-snorm 18
                            :r16-snorm 19
                            :r8-snorm 20
                            :rgba32i 21
                            :rgba16i 22
                            :rgba8i 23
                            :r32i 24
                            :rg32i 25
                            :rg16i 26
                            :rg8i 27
                            :r16i 28
                            :r8i 29
                            :rgba32ui 30
                            :rgba16ui 31
                            :rgba8ui 32
                            :r32ui 33
                            :rgb10a2ui 34
                            :rg32ui 35
                            :rg16ui 36
                            :rg8ui 37
                            :r16ui 38
                            :r8ui 39)))
               :image-operands
               (list :bit
                     (alexandria:plist-hash-table
                      (list :bias 0
                            :lod 1
                            :grad 2
                            :const-offset 3
                            :offset 4
                            :const-offsets 5
                            :sample 6
                            :min-lod 7)))
               :kernel-enqueue-flags
               (list :value
                     (alexandria:plist-hash-table
                      (list :no-wait 0
                            :wait-kernel 1
                            :wait-work-group 2)))
               :kernel-profiling-info
               (list :bit
                     (alexandria:plist-hash-table (list :cmd-exec-time 0)))
               :linkage-type
               (list :value
                     (alexandria:plist-hash-table
                      (list :export 0
                            :import 1)))
               :loop-control
               (list :bit
                     (alexandria:plist-hash-table
                      (list :unroll 0
                            :dont-unroll 1
                            :dependency-infinite 2
                            :dependency-length 3)))
               :memory-access
               (list :bit
                     (alexandria:plist-hash-table
                      (list :volatile 0
                            :aligned 1
                            :nontemporal 2)))
               :memory-model
               (list :value
                     (alexandria:plist-hash-table
                      (list :simple 0
                            :glsl-450 1
                            :opencl 2)))
               :memory-semantics
               (list :bit
                     (alexandria:plist-hash-table
                      (list :acquire 1
                            :release 2
                            :acquire-release 3
                            :sequentially-consistent 4
                            :uniform-memory 6
                            :subgroup-memory 7
                            :workgroup-memory 8
                            :cross-workgroup-memory 9
                            :atomic-counter-memory 10
                            :image-memory 11)))
               :op
               (list :value
                     (alexandria:plist-hash-table
                      (list 'spirv-core:nop 0
                            'spirv-core:undef 1
                            'spirv-core:source-continued 2
                            'spirv-core:source 3
                            'spirv-core:source-extension 4
                            'spirv-core:name 5
                            'spirv-core:member-name 6
                            'spirv-core:string 7
                            'spirv-core:line 8
                            'spirv-core:extension 10
                            'spirv-core:ext-inst-import 11
                            'spirv-core:ext-inst 12
                            'spirv-core:memory-model 14
                            'spirv-core:entry-point 15
                            'spirv-core:execution-mode 16
                            'spirv-core:capability 17
                            'spirv-core:type-void 19
                            'spirv-core:type-bool 20
                            'spirv-core:type-int 21
                            'spirv-core:type-float 22
                            'spirv-core:type-vector 23
                            'spirv-core:type-matrix 24
                            'spirv-core:type-image 25
                            'spirv-core:type-sampler 26
                            'spirv-core:type-sampled-image 27
                            'spirv-core:type-array 28
                            'spirv-core:type-runtime-array 29
                            'spirv-core:type-struct 30
                            'spirv-core:type-opaque 31
                            'spirv-core:type-pointer 32
                            'spirv-core:type-function 33
                            'spirv-core:type-event 34
                            'spirv-core:type-device-event 35
                            'spirv-core:type-reserve-id 36
                            'spirv-core:type-queue 37
                            'spirv-core:type-pipe 38
                            'spirv-core:type-forward-pointer 39
                            'spirv-core:constant-true 41
                            'spirv-core:constant-false 42
                            'spirv-core:constant 43
                            'spirv-core:constant-composite 44
                            'spirv-core:constant-sampler 45
                            'spirv-core:constant-null 46
                            'spirv-core:spec-constant-true 48
                            'spirv-core:spec-constant-false 49
                            'spirv-core:spec-constant 50
                            'spirv-core:spec-constant-composite 51
                            'spirv-core:spec-constant-op 52
                            'spirv-core:function 54
                            'spirv-core:function-parameter 55
                            'spirv-core:function-end 56
                            'spirv-core:function-call 57
                            'spirv-core:variable 59
                            'spirv-core:image-texel-pointer 60
                            'spirv-core:load 61
                            'spirv-core:store 62
                            'spirv-core:copy-memory 63
                            'spirv-core:copy-memory-sized 64
                            'spirv-core:access-chain 65
                            'spirv-core:in-bounds-access-chain 66
                            'spirv-core:ptr-access-chain 67
                            'spirv-core:array-length 68
                            'spirv-core:generic-ptr-mem-semantics 69
                            'spirv-core:in-bounds-ptr-access-chain 70
                            'spirv-core:decorate 71
                            'spirv-core:member-decorate 72
                            'spirv-core:decoration-group 73
                            'spirv-core:group-decorate 74
                            'spirv-core:group-member-decorate 75
                            'spirv-core:vector-extract-dynamic 77
                            'spirv-core:vector-insert-dynamic 78
                            'spirv-core:vector-shuffle 79
                            'spirv-core:composite-construct 80
                            'spirv-core:composite-extract 81
                            'spirv-core:composite-insert 82
                            'spirv-core:copy-object 83
                            'spirv-core:transpose 84
                            'spirv-core:sampled-image 86
                            'spirv-core:image-sample-implicit-lod 87
                            'spirv-core:image-sample-explicit-lod 88
                            'spirv-core:image-sample-dref-implicit-lod 89
                            'spirv-core:image-sample-dref-explicit-lod 90
                            'spirv-core:image-sample-proj-implicit-lod 91
                            'spirv-core:image-sample-proj-explicit-lod 92
                            'spirv-core:image-sample-proj-dref-implicit-lod 93
                            'spirv-core:image-sample-proj-dref-explicit-lod 94
                            'spirv-core:image-fetch 95
                            'spirv-core:image-gather 96
                            'spirv-core:image-dref-gather 97
                            'spirv-core:image-read 98
                            'spirv-core:image-write 99
                            'spirv-core:image 100
                            'spirv-core:image-query-format 101
                            'spirv-core:image-query-order 102
                            'spirv-core:image-query-size-lod 103
                            'spirv-core:image-query-size 104
                            'spirv-core:image-query-lod 105
                            'spirv-core:image-query-levels 106
                            'spirv-core:image-query-samples 107
                            'spirv-core:convert-f-to-u 109
                            'spirv-core:convert-f-to-s 110
                            'spirv-core:convert-s-to-f 111
                            'spirv-core:convert-u-to-f 112
                            'spirv-core:u-convert 113
                            'spirv-core:s-convert 114
                            'spirv-core:f-convert 115
                            'spirv-core:quantize-to-f16 116
                            'spirv-core:convert-ptr-to-u 117
                            'spirv-core:sat-convert-s-to-u 118
                            'spirv-core:sat-convert-u-to-s 119
                            'spirv-core:convert-u-to-ptr 120
                            'spirv-core:ptr-cast-to-generic 121
                            'spirv-core:generic-cast-to-ptr 122
                            'spirv-core:generic-cast-to-ptr-explicit 123
                            'spirv-core:bitcast 124
                            'spirv-core:s-negate 126
                            'spirv-core:f-negate 127
                            'spirv-core:i-add 128
                            'spirv-core:f-add 129
                            'spirv-core:i-sub 130
                            'spirv-core:f-sub 131
                            'spirv-core:i-mul 132
                            'spirv-core:f-mul 133
                            'spirv-core:u-div 134
                            'spirv-core:s-div 135
                            'spirv-core:f-div 136
                            'spirv-core:u-mod 137
                            'spirv-core:s-rem 138
                            'spirv-core:s-mod 139
                            'spirv-core:f-rem 140
                            'spirv-core:f-mod 141
                            'spirv-core:vector-times-scalar 142
                            'spirv-core:matrix-times-scalar 143
                            'spirv-core:vector-times-matrix 144
                            'spirv-core:matrix-times-vector 145
                            'spirv-core:matrix-times-matrix 146
                            'spirv-core:outer-product 147
                            'spirv-core:dot 148
                            'spirv-core:i-add-carry 149
                            'spirv-core:i-sub-borrow 150
                            'spirv-core:u-mul-extended 151
                            'spirv-core:s-mul-extended 152
                            'spirv-core:any 154
                            'spirv-core:all 155
                            'spirv-core:is-nan 156
                            'spirv-core:is-inf 157
                            'spirv-core:is-finite 158
                            'spirv-core:is-normal 159
                            'spirv-core:sign-bit-set 160
                            'spirv-core:less-or-greater 161
                            'spirv-core:ordered 162
                            'spirv-core:unordered 163
                            'spirv-core:logical-equal 164
                            'spirv-core:logical-not-equal 165
                            'spirv-core:logical-or 166
                            'spirv-core:logical-and 167
                            'spirv-core:logical-not 168
                            'spirv-core:select 169
                            'spirv-core:i-equal 170
                            'spirv-core:i-not-equal 171
                            'spirv-core:u-greater-than 172
                            'spirv-core:s-greater-than 173
                            'spirv-core:u-greater-than-equal 174
                            'spirv-core:s-greater-than-equal 175
                            'spirv-core:u-less-than 176
                            'spirv-core:s-less-than 177
                            'spirv-core:u-less-than-equal 178
                            'spirv-core:s-less-than-equal 179
                            'spirv-core:f-ord-equal 180
                            'spirv-core:f-unord-equal 181
                            'spirv-core:f-ord-not-equal 182
                            'spirv-core:f-unord-not-equal 183
                            'spirv-core:f-ord-less-than 184
                            'spirv-core:f-unord-less-than 185
                            'spirv-core:f-ord-greater-than 186
                            'spirv-core:f-unord-greater-than 187
                            'spirv-core:f-ord-less-than-equal 188
                            'spirv-core:f-unord-less-than-equal 189
                            'spirv-core:f-ord-greater-than-equal 190
                            'spirv-core:f-unord-greater-than-equal 191
                            'spirv-core:shift-right-logical 194
                            'spirv-core:shift-right-arithmetic 195
                            'spirv-core:shift-left-logical 196
                            'spirv-core:bitwise-or 197
                            'spirv-core:bitwise-xor 198
                            'spirv-core:bitwise-and 199
                            'spirv-core:not 200
                            'spirv-core:bit-field-insert 201
                            'spirv-core:bit-field-s-extract 202
                            'spirv-core:bit-field-u-extract 203
                            'spirv-core:bit-reverse 204
                            'spirv-core:bit-count 205
                            'spirv-core:dp-dx 207
                            'spirv-core:dp-dy 208
                            'spirv-core:fwidth 209
                            'spirv-core:dp-dx-fine 210
                            'spirv-core:dp-dy-fine 211
                            'spirv-core:fwidth-fine 212
                            'spirv-core:dp-dx-coarse 213
                            'spirv-core:dp-dy-coarse 214
                            'spirv-core:fwidth-coarse 215
                            'spirv-core:emit-vertex 218
                            'spirv-core:end-primitive 219
                            'spirv-core:emit-stream-vertex 220
                            'spirv-core:end-stream-primitive 221
                            'spirv-core:control-barrier 224
                            'spirv-core:memory-barrier 225
                            'spirv-core:atomic-load 227
                            'spirv-core:atomic-store 228
                            'spirv-core:atomic-exchange 229
                            'spirv-core:atomic-compare-exchange 230
                            'spirv-core:atomic-compare-exchange-weak 231
                            'spirv-core:atomic-i-increment 232
                            'spirv-core:atomic-i-decrement 233
                            'spirv-core:atomic-i-add 234
                            'spirv-core:atomic-i-sub 235
                            'spirv-core:atomic-s-min 236
                            'spirv-core:atomic-u-min 237
                            'spirv-core:atomic-s-max 238
                            'spirv-core:atomic-u-max 239
                            'spirv-core:atomic-and 240
                            'spirv-core:atomic-or 241
                            'spirv-core:atomic-xor 242
                            'spirv-core:phi 245
                            'spirv-core:loop-merge 246
                            'spirv-core:selection-merge 247
                            'spirv-core:label 248
                            'spirv-core:branch 249
                            'spirv-core:branch-conditional 250
                            'spirv-core:switch 251
                            'spirv-core:kill 252
                            'spirv-core:return 253
                            'spirv-core:return-value 254
                            'spirv-core:unreachable 255
                            'spirv-core:lifetime-start 256
                            'spirv-core:lifetime-stop 257
                            'spirv-core:group-async-copy 259
                            'spirv-core:group-wait-events 260
                            'spirv-core:group-all 261
                            'spirv-core:group-any 262
                            'spirv-core:group-broadcast 263
                            'spirv-core:group-i-add 264
                            'spirv-core:group-f-add 265
                            'spirv-core:group-f-min 266
                            'spirv-core:group-u-min 267
                            'spirv-core:group-s-min 268
                            'spirv-core:group-f-max 269
                            'spirv-core:group-u-max 270
                            'spirv-core:group-s-max 271
                            'spirv-core:read-pipe 274
                            'spirv-core:write-pipe 275
                            'spirv-core:reserved-read-pipe 276
                            'spirv-core:reserved-write-pipe 277
                            'spirv-core:reserve-read-pipe-packets 278
                            'spirv-core:reserve-write-pipe-packets 279
                            'spirv-core:commit-read-pipe 280
                            'spirv-core:commit-write-pipe 281
                            'spirv-core:is-valid-reserve-id 282
                            'spirv-core:get-num-pipe-packets 283
                            'spirv-core:get-max-pipe-packets 284
                            'spirv-core:group-reserve-read-pipe-packets 285
                            'spirv-core:group-reserve-write-pipe-packets 286
                            'spirv-core:group-commit-read-pipe 287
                            'spirv-core:group-commit-write-pipe 288
                            'spirv-core:enqueue-marker 291
                            'spirv-core:enqueue-kernel 292
                            'spirv-core:get-kernel-nd-range-sub-group-count 293
                            'spirv-core:get-kernel-nd-range-max-sub-group-size 294
                            'spirv-core:get-kernel-work-group-size 295
                            'spirv-core:get-kernel-preferred-work-group-size-multiple
                            296
                            'spirv-core:retain-event 297
                            'spirv-core:release-event 298
                            'spirv-core:create-user-event 299
                            'spirv-core:is-valid-event 300
                            'spirv-core:set-user-event-status 301
                            'spirv-core:capture-event-profiling-info 302
                            'spirv-core:get-default-queue 303
                            'spirv-core:build-nd-range 304
                            'spirv-core:image-sparse-sample-implicit-lod 305
                            'spirv-core:image-sparse-sample-explicit-lod 306
                            'spirv-core:image-sparse-sample-dref-implicit-lod 307
                            'spirv-core:image-sparse-sample-dref-explicit-lod 308
                            'spirv-core:image-sparse-sample-proj-implicit-lod 309
                            'spirv-core:image-sparse-sample-proj-explicit-lod 310
                            'spirv-core:image-sparse-sample-proj-dref-implicit-lod
                            311
                            'spirv-core:image-sparse-sample-proj-dref-explicit-lod
                            312
                            'spirv-core:image-sparse-fetch 313
                            'spirv-core:image-sparse-gather 314
                            'spirv-core:image-sparse-dref-gather 315
                            'spirv-core:image-sparse-texels-resident 316
                            'spirv-core:no-line 317
                            'spirv-core:atomic-flag-test-and-set 318
                            'spirv-core:atomic-flag-clear 319
                            'spirv-core:image-sparse-read 320
                            'spirv-core:size-of 321
                            'spirv-core:type-pipe-storage 322
                            'spirv-core:constant-pipe-storage 323
                            'spirv-core:create-pipe-from-pipe-storage 324
                            'spirv-core:get-kernel-local-size-for-subgroup-count 325
                            'spirv-core:get-kernel-max-num-subgroups 326
                            'spirv-core:type-named-barrier 327
                            'spirv-core:named-barrier-initialize 328
                            'spirv-core:memory-named-barrier 329
                            'spirv-core:module-processed 330
                            'spirv-core:subgroup-ballot-khr 4421
                            'spirv-core:subgroup-first-invocation-khr 4422)))
               :sampler-addressing-mode
               (list :value
                     (alexandria:plist-hash-table
                      (list :none 0
                            :clamp-to-edge 1
                            :clamp 2
                            :repeat 3
                            :repeat-mirrored 4)))
               :sampler-filter-mode
               (list :value
                     (alexandria:plist-hash-table
                      (list :nearest 0
                            :linear 1)))
               :scope
               (list :value
                     (alexandria:plist-hash-table
                      (list :cross-device 0
                            :device 1
                            :workgroup 2
                            :subgroup 3
                            :invocation 4)))
               :selection-control
               (list :bit
                     (alexandria:plist-hash-table
                      (list :flatten 0
                            :dont-flatten 1)))
               :source-language
               (list :value
                     (alexandria:plist-hash-table
                      (list :unknown 0
                            :essl 1
                            :glsl 2
                            :opencl-c 3
                            :opencl-cpp 4)))
               :storage-class
               (list :value
                     (alexandria:plist-hash-table
                      (list :uniform-constant 0
                            :input 1
                            :uniform 2
                            :output 3
                            :workgroup 4
                            :cross-workgroup 5
                            :private 6
                            :function 7
                            :generic 8
                            :push-constant 9
                            :atomic-counter 10
                            :image 11)))))

        :opcodes
        (alexandria:plist-hash-table
         (list 'spirv-core:nop
               (list :value 0 :required-capabilities nil :operands nil)
               'spirv-core:undef
               (list :value 1 :required-capabilities nil :resultp :typed
                     :operands nil)
               'spirv-core:source-continued
               (list :value 2 :required-capabilities nil :operands
                     (list (list :literal-string "'continued source'")))
               'spirv-core:source
               (list :value 3 :required-capabilities nil :operands
                     (list (list :source-language)
                           (list :literal-integer "'version'")
                           (list (list :? :id-ref) "'file'")
                           (list (list :? :literal-string) "'source'")))
               'spirv-core:source-extension
               (list :value 4 :required-capabilities nil :operands
                     (list (list :literal-string "'extension'")))
               'spirv-core:name
               (list :value 5 :required-capabilities nil :operands
                     (list (list :id-ref "'target'")
                           (list :literal-string "'name'")))
               'spirv-core:member-name
               (list :value 6 :required-capabilities nil :operands
                     (list (list :id-ref "'type'")
                           (list :literal-integer "'member'")
                           (list :literal-string "'name'")))
               'spirv-core:string
               (list :value 7 :required-capabilities nil :resultp t :operands
                     (list (list :literal-string "'string'")))
               'spirv-core:line
               (list :value 8 :required-capabilities nil :operands
                     (list (list :id-ref "'file'") (list :literal-integer "'line'")
                           (list :literal-integer "'column'")))
               'spirv-core:extension
               (list :value 10 :required-capabilities nil :operands
                     (list (list :literal-string "'name'")))
               'spirv-core:ext-inst-import
               (list :value 11 :required-capabilities nil :resultp t :operands
                     (list (list :literal-string "'name'")))
               'spirv-core:ext-inst
               (list :value 12 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'set'")
                           (list :literal-ext-inst-integer "'instruction'")
                           (list (list :* :id-ref) "'operand 1', +
'operand 2', +
...")))
               'spirv-core:memory-model
               (list :value 14 :required-capabilities nil :operands
                     (list (list :addressing-model) (list :memory-model)))
               'spirv-core:entry-point
               (list :value 15 :required-capabilities nil :operands
                     (list (list :execution-model) (list :id-ref "'entry point'")
                           (list :literal-string "'name'")
                           (list (list :* :id-ref) "'interface'")))
               'spirv-core:execution-mode
               (list :value 16 :required-capabilities nil :operands
                     (list (list :id-ref "'entry point'")
                           (list :execution-mode "'mode'")))
               'spirv-core:capability
               (list :value 17 :required-capabilities nil :operands
                     (list (list :capability "'capability'")))
               'spirv-core:type-void
               (list :value 19 :required-capabilities nil :resultp t :operands
                     nil)
               'spirv-core:type-bool
               (list :value 20 :required-capabilities nil :resultp t :operands
                     nil)
               'spirv-core:type-int
               (list :value 21 :required-capabilities nil :resultp t :operands
                     (list (list :literal-integer "'width'")
                           (list :literal-integer "'signedness'")))
               'spirv-core:type-float
               (list :value 22 :required-capabilities nil :resultp t :operands
                     (list (list :literal-integer "'width'")))
               'spirv-core:type-vector
               (list :value 23 :required-capabilities nil :resultp t :operands
                     (list (list :id-ref "'component type'")
                           (list :literal-integer "'component count'")))
               'spirv-core:type-matrix
               (list :value 24 :required-capabilities (list :matrix) :resultp
                     t :operands
                     (list (list :id-ref "'column type'")
                           (list :literal-integer "'column count'")))
               'spirv-core:type-image
               (list :value 25 :required-capabilities nil :resultp t :operands
                     (list (list :id-ref "'sampled type'") (list :dim)
                           (list :literal-integer "'depth'")
                           (list :literal-integer "'arrayed'")
                           (list :literal-integer "'ms'")
                           (list :literal-integer "'sampled'") (list :image-format)
                           (list (list :? :access-qualifier))))
               'spirv-core:type-sampler
               (list :value 26 :required-capabilities nil :resultp t :operands
                     nil)
               'spirv-core:type-sampled-image
               (list :value 27 :required-capabilities nil :resultp t :operands
                     (list (list :id-ref "'image type'")))
               'spirv-core:type-array
               (list :value 28 :required-capabilities nil :resultp t :operands
                     (list (list :id-ref "'element type'")
                           (list :id-ref "'length'")))
               'spirv-core:type-runtime-array
               (list :value 29 :required-capabilities (list :shader) :resultp
                     t :operands (list (list :id-ref "'element type'")))
               'spirv-core:type-struct
               (list :value 30 :required-capabilities nil :resultp t :operands
                     (list
                      (list (list :* :id-ref) "'member 0 type', +
'member 1 type', +
...")))
               'spirv-core:type-opaque
               (list :value 31 :required-capabilities (list :kernel) :resultp
                     t :operands
                     (list (list :literal-string "the name of the opaque type.")))
               'spirv-core:type-pointer
               (list :value 32 :required-capabilities nil :resultp t :operands
                     (list (list :storage-class) (list :id-ref "'type'")))
               'spirv-core:type-function
               (list :value 33 :required-capabilities nil :resultp t :operands
                     (list (list :id-ref "'return type'")
                           (list (list :* :id-ref) "'parameter 0 type', +
'parameter 1 type', +
...")))
               'spirv-core:type-event
               (list :value 34 :required-capabilities (list :kernel) :resultp
                     t :operands nil)
               'spirv-core:type-device-event
               (list :value 35 :required-capabilities (list :device-enqueue)
                     :resultp t :operands nil)
               'spirv-core:type-reserve-id
               (list :value 36 :required-capabilities (list :pipes) :resultp t
                     :operands nil)
               'spirv-core:type-queue
               (list :value 37 :required-capabilities (list :device-enqueue)
                     :resultp t :operands nil)
               'spirv-core:type-pipe
               (list :value 38 :required-capabilities (list :pipes) :resultp t
                     :operands (list (list :access-qualifier "'qualifier'")))
               'spirv-core:type-forward-pointer
               (list :value 39 :required-capabilities (list :addresses)
                     :operands
                     (list (list :id-ref "'pointer type'") (list :storage-class)))
               'spirv-core:constant-true
               (list :value 41 :required-capabilities nil :resultp :typed
                     :operands nil)
               'spirv-core:constant-false
               (list :value 42 :required-capabilities nil :resultp :typed
                     :operands nil)
               'spirv-core:constant
               (list :value 43 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :literal-context-dependent-number "'value'")))
               'spirv-core:constant-composite
               (list :value 44 :required-capabilities nil :resultp :typed
                     :operands (list (list (list :* :id-ref) "'constituents'")))
               'spirv-core:constant-sampler
               (list :value 45 :required-capabilities (list :literal-sampler)
                     :resultp :typed :operands
                     (list (list :sampler-addressing-mode)
                           (list :literal-integer "'param'")
                           (list :sampler-filter-mode)))
               'spirv-core:constant-null
               (list :value 46 :required-capabilities nil :resultp :typed
                     :operands nil)
               'spirv-core:spec-constant-true
               (list :value 48 :required-capabilities nil :resultp :typed
                     :operands nil)
               'spirv-core:spec-constant-false
               (list :value 49 :required-capabilities nil :resultp :typed
                     :operands nil)
               'spirv-core:spec-constant
               (list :value 50 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :literal-context-dependent-number "'value'")))
               'spirv-core:spec-constant-composite
               (list :value 51 :required-capabilities nil :resultp :typed
                     :operands (list (list (list :* :id-ref) "'constituents'")))
               'spirv-core:spec-constant-op
               (list :value 52 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :literal-spec-constant-op-integer "'opcode'")))
               'spirv-core:function
               (list :value 54 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :function-control)
                           (list :id-ref "'function type'")))
               'spirv-core:function-parameter
               (list :value 55 :required-capabilities nil :resultp :typed
                     :operands nil)
               'spirv-core:function-end
               (list :value 56 :required-capabilities nil :operands nil)
               'spirv-core:function-call
               (list :value 57 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'function'")
                           (list (list :* :id-ref) "'argument 0', +
'argument 1', +
...")))
               'spirv-core:variable
               (list :value 59 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :storage-class)
                           (list (list :? :id-ref) "'initializer'")))
               'spirv-core:image-texel-pointer
               (list :value 60 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'image'") (list :id-ref "'coordinate'")
                           (list :id-ref "'sample'")))
               'spirv-core:load
               (list :value 61 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'")
                           (list (list :? :memory-access))))
               'spirv-core:store
               (list :value 62 :required-capabilities nil :operands
                     (list (list :id-ref "'pointer'") (list :id-ref "'object'")
                           (list (list :? :memory-access))))
               'spirv-core:copy-memory
               (list :value 63 :required-capabilities nil :operands
                     (list (list :id-ref "'target'") (list :id-ref "'source'")
                           (list (list :? :memory-access))))
               'spirv-core:copy-memory-sized
               (list :value 64 :required-capabilities (list :addresses)
                     :operands
                     (list (list :id-ref "'target'") (list :id-ref "'source'")
                           (list :id-ref "'size'") (list (list :? :memory-access))))
               'spirv-core:access-chain
               (list :value 65 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'base'")
                           (list (list :* :id-ref) "'indexes'")))
               'spirv-core:in-bounds-access-chain
               (list :value 66 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'base'")
                           (list (list :* :id-ref) "'indexes'")))
               'spirv-core:ptr-access-chain
               (list :value 67 :required-capabilities (list :addresses)
                     :resultp :typed :operands
                     (list (list :id-ref "'base'") (list :id-ref "'element'")
                           (list (list :* :id-ref) "'indexes'")))
               'spirv-core:array-length
               (list :value 68 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'structure'")
                           (list :literal-integer "'array member'")))
               'spirv-core:generic-ptr-mem-semantics
               (list :value 69 :required-capabilities (list :kernel) :resultp
                     :typed :operands (list (list :id-ref "'pointer'")))
               'spirv-core:in-bounds-ptr-access-chain
               (list :value 70 :required-capabilities (list :addresses)
                     :resultp :typed :operands
                     (list (list :id-ref "'base'") (list :id-ref "'element'")
                           (list (list :* :id-ref) "'indexes'")))
               'spirv-core:decorate
               (list :value 71 :required-capabilities nil :operands
                     (list (list :id-ref "'target'") (list :decoration)))
               'spirv-core:member-decorate
               (list :value 72 :required-capabilities nil :operands
                     (list (list :id-ref "'structure type'")
                           (list :literal-integer "'member'") (list :decoration)))
               'spirv-core:decoration-group
               (list :value 73 :required-capabilities nil :resultp t :operands
                     nil)
               'spirv-core:group-decorate
               (list :value 74 :required-capabilities nil :operands
                     (list (list :id-ref "'decoration group'")
                           (list (list :* :id-ref) "'targets'")))
               'spirv-core:group-member-decorate
               (list :value 75 :required-capabilities nil :operands
                     (list (list :id-ref "'decoration group'")
                           (list (list :* :pair-id-ref-literal-integer) "'targets'")))
               'spirv-core:vector-extract-dynamic
               (list :value 77 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'vector'") (list :id-ref "'index'")))
               'spirv-core:vector-insert-dynamic
               (list :value 78 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'vector'") (list :id-ref "'component'")
                           (list :id-ref "'index'")))
               'spirv-core:vector-shuffle
               (list :value 79 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'vector 1'") (list :id-ref "'vector 2'")
                           (list (list :* :literal-integer) "'components'")))
               'spirv-core:composite-construct
               (list :value 80 :required-capabilities nil :resultp :typed
                     :operands (list (list (list :* :id-ref) "'constituents'")))
               'spirv-core:composite-extract
               (list :value 81 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'composite'")
                           (list (list :* :literal-integer) "'indexes'")))
               'spirv-core:composite-insert
               (list :value 82 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'object'") (list :id-ref "'composite'")
                           (list (list :* :literal-integer) "'indexes'")))
               'spirv-core:copy-object
               (list :value 83 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'operand'")))
               'spirv-core:transpose
               (list :value 84 :required-capabilities (list :matrix) :resultp
                     :typed :operands (list (list :id-ref "'matrix'")))
               'spirv-core:sampled-image
               (list :value 86 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'image'") (list :id-ref "'sampler'")))
               'spirv-core:image-sample-implicit-lod
               (list :value 87 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sample-explicit-lod
               (list :value 88 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :image-operands)))
               'spirv-core:image-sample-dref-implicit-lod
               (list :value 89 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'d~ref~'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sample-dref-explicit-lod
               (list :value 90 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'d~ref~'")
                           (list :image-operands)))
               'spirv-core:image-sample-proj-implicit-lod
               (list :value 91 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sample-proj-explicit-lod
               (list :value 92 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :image-operands)))
               'spirv-core:image-sample-proj-dref-implicit-lod
               (list :value 93 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'d~ref~'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sample-proj-dref-explicit-lod
               (list :value 94 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'d~ref~'")
                           (list :image-operands)))
               'spirv-core:image-fetch
               (list :value 95 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'image'") (list :id-ref "'coordinate'")
                           (list (list :? :image-operands))))
               'spirv-core:image-gather
               (list :value 96 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'component'")
                           (list (list :? :image-operands))))
               'spirv-core:image-dref-gather
               (list :value 97 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'d~ref~'")
                           (list (list :? :image-operands))))
               'spirv-core:image-read
               (list :value 98 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'image'") (list :id-ref "'coordinate'")
                           (list (list :? :image-operands))))
               'spirv-core:image-write
               (list :value 99 :required-capabilities nil :operands
                     (list (list :id-ref "'image'") (list :id-ref "'coordinate'")
                           (list :id-ref "'texel'") (list (list :? :image-operands))))
               'spirv-core:image
               (list :value 100 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'sampled image'")))
               'spirv-core:image-query-format
               (list :value 101 :required-capabilities (list :kernel) :resultp
                     :typed :operands (list (list :id-ref "'image'")))
               'spirv-core:image-query-order
               (list :value 102 :required-capabilities (list :kernel) :resultp
                     :typed :operands (list (list :id-ref "'image'")))
               'spirv-core:image-query-size-lod
               (list :value 103 :required-capabilities
                     (list :kernel :image-query) :resultp :typed :operands
                     (list (list :id-ref "'image'")
                           (list :id-ref "'level of detail'")))
               'spirv-core:image-query-size
               (list :value 104 :required-capabilities
                     (list :kernel :image-query) :resultp :typed :operands
                     (list (list :id-ref "'image'")))
               'spirv-core:image-query-lod
               (list :value 105 :required-capabilities (list :image-query)
                     :resultp :typed :operands
                     (list (list :id-ref "'image'") (list :id-ref "'coordinate'")))
               'spirv-core:image-query-levels
               (list :value 106 :required-capabilities
                     (list :kernel :image-query) :resultp :typed :operands
                     (list (list :id-ref "'image'")))
               'spirv-core:image-query-samples
               (list :value 107 :required-capabilities
                     (list :kernel :image-query) :resultp :typed :operands
                     (list (list :id-ref "'image'")))
               'spirv-core:convert-f-to-u
               (list :value 109 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'float value'")))
               'spirv-core:convert-f-to-s
               (list :value 110 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'float value'")))
               'spirv-core:convert-s-to-f
               (list :value 111 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'signed value'")))
               'spirv-core:convert-u-to-f
               (list :value 112 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'unsigned value'")))
               'spirv-core:u-convert
               (list :value 113 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'unsigned value'")))
               'spirv-core:s-convert
               (list :value 114 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'signed value'")))
               'spirv-core:f-convert
               (list :value 115 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'float value'")))
               'spirv-core:quantize-to-f16
               (list :value 116 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'value'")))
               'spirv-core:convert-ptr-to-u
               (list :value 117 :required-capabilities (list :addresses)
                     :resultp :typed :operands (list (list :id-ref "'pointer'")))
               'spirv-core:sat-convert-s-to-u
               (list :value 118 :required-capabilities (list :kernel) :resultp
                     :typed :operands (list (list :id-ref "'signed value'")))
               'spirv-core:sat-convert-u-to-s
               (list :value 119 :required-capabilities (list :kernel) :resultp
                     :typed :operands (list (list :id-ref "'unsigned value'")))
               'spirv-core:convert-u-to-ptr
               (list :value 120 :required-capabilities (list :addresses)
                     :resultp :typed :operands
                     (list (list :id-ref "'integer value'")))
               'spirv-core:ptr-cast-to-generic
               (list :value 121 :required-capabilities (list :kernel) :resultp
                     :typed :operands (list (list :id-ref "'pointer'")))
               'spirv-core:generic-cast-to-ptr
               (list :value 122 :required-capabilities (list :kernel) :resultp
                     :typed :operands (list (list :id-ref "'pointer'")))
               'spirv-core:generic-cast-to-ptr-explicit
               (list :value 123 :required-capabilities (list :kernel) :resultp
                     :typed :operands
                     (list (list :id-ref "'pointer'")
                           (list :storage-class "'storage'")))
               'spirv-core:bitcast
               (list :value 124 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'operand'")))
               'spirv-core:s-negate
               (list :value 126 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'operand'")))
               'spirv-core:f-negate
               (list :value 127 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'operand'")))
               'spirv-core:i-add
               (list :value 128 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-add
               (list :value 129 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:i-sub
               (list :value 130 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-sub
               (list :value 131 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:i-mul
               (list :value 132 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-mul
               (list :value 133 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:u-div
               (list :value 134 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:s-div
               (list :value 135 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-div
               (list :value 136 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:u-mod
               (list :value 137 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:s-rem
               (list :value 138 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:s-mod
               (list :value 139 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-rem
               (list :value 140 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-mod
               (list :value 141 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:vector-times-scalar
               (list :value 142 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'vector'") (list :id-ref "'scalar'")))
               'spirv-core:matrix-times-scalar
               (list :value 143 :required-capabilities (list :matrix) :resultp
                     :typed :operands
                     (list (list :id-ref "'matrix'") (list :id-ref "'scalar'")))
               'spirv-core:vector-times-matrix
               (list :value 144 :required-capabilities (list :matrix) :resultp
                     :typed :operands
                     (list (list :id-ref "'vector'") (list :id-ref "'matrix'")))
               'spirv-core:matrix-times-vector
               (list :value 145 :required-capabilities (list :matrix) :resultp
                     :typed :operands
                     (list (list :id-ref "'matrix'") (list :id-ref "'vector'")))
               'spirv-core:matrix-times-matrix
               (list :value 146 :required-capabilities (list :matrix) :resultp
                     :typed :operands
                     (list (list :id-ref "'leftmatrix'")
                           (list :id-ref "'rightmatrix'")))
               'spirv-core:outer-product
               (list :value 147 :required-capabilities (list :matrix) :resultp
                     :typed :operands
                     (list (list :id-ref "'vector 1'") (list :id-ref "'vector 2'")))
               'spirv-core:dot
               (list :value 148 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'vector 1'") (list :id-ref "'vector 2'")))
               'spirv-core:i-add-carry
               (list :value 149 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:i-sub-borrow
               (list :value 150 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:u-mul-extended
               (list :value 151 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:s-mul-extended
               (list :value 152 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:any
               (list :value 154 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'vector'")))
               'spirv-core:all
               (list :value 155 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'vector'")))
               'spirv-core:is-nan
               (list :value 156 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'x'")))
               'spirv-core:is-inf
               (list :value 157 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'x'")))
               'spirv-core:is-finite
               (list :value 158 :required-capabilities (list :kernel) :resultp
                     :typed :operands (list (list :id-ref "'x'")))
               'spirv-core:is-normal
               (list :value 159 :required-capabilities (list :kernel) :resultp
                     :typed :operands (list (list :id-ref "'x'")))
               'spirv-core:sign-bit-set
               (list :value 160 :required-capabilities (list :kernel) :resultp
                     :typed :operands (list (list :id-ref "'x'")))
               'spirv-core:less-or-greater
               (list :value 161 :required-capabilities (list :kernel) :resultp
                     :typed :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-core:ordered
               (list :value 162 :required-capabilities (list :kernel) :resultp
                     :typed :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-core:unordered
               (list :value 163 :required-capabilities (list :kernel) :resultp
                     :typed :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-core:logical-equal
               (list :value 164 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:logical-not-equal
               (list :value 165 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:logical-or
               (list :value 166 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:logical-and
               (list :value 167 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:logical-not
               (list :value 168 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'operand'")))
               'spirv-core:select
               (list :value 169 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'condition'") (list :id-ref "'object 1'")
                           (list :id-ref "'object 2'")))
               'spirv-core:i-equal
               (list :value 170 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:i-not-equal
               (list :value 171 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:u-greater-than
               (list :value 172 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:s-greater-than
               (list :value 173 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:u-greater-than-equal
               (list :value 174 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:s-greater-than-equal
               (list :value 175 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:u-less-than
               (list :value 176 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:s-less-than
               (list :value 177 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:u-less-than-equal
               (list :value 178 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:s-less-than-equal
               (list :value 179 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-ord-equal
               (list :value 180 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-unord-equal
               (list :value 181 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-ord-not-equal
               (list :value 182 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-unord-not-equal
               (list :value 183 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-ord-less-than
               (list :value 184 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-unord-less-than
               (list :value 185 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-ord-greater-than
               (list :value 186 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-unord-greater-than
               (list :value 187 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-ord-less-than-equal
               (list :value 188 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-unord-less-than-equal
               (list :value 189 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-ord-greater-than-equal
               (list :value 190 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:f-unord-greater-than-equal
               (list :value 191 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:shift-right-logical
               (list :value 194 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'base'") (list :id-ref "'shift'")))
               'spirv-core:shift-right-arithmetic
               (list :value 195 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'base'") (list :id-ref "'shift'")))
               'spirv-core:shift-left-logical
               (list :value 196 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'base'") (list :id-ref "'shift'")))
               'spirv-core:bitwise-or
               (list :value 197 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:bitwise-xor
               (list :value 198 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:bitwise-and
               (list :value 199 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'operand 1'")
                           (list :id-ref "'operand 2'")))
               'spirv-core:not
               (list :value 200 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'operand'")))
               'spirv-core:bit-field-insert
               (list :value 201 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'base'") (list :id-ref "'insert'")
                           (list :id-ref "'offset'") (list :id-ref "'count'")))
               'spirv-core:bit-field-s-extract
               (list :value 202 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'base'") (list :id-ref "'offset'")
                           (list :id-ref "'count'")))
               'spirv-core:bit-field-u-extract
               (list :value 203 :required-capabilities (list :shader) :resultp
                     :typed :operands
                     (list (list :id-ref "'base'") (list :id-ref "'offset'")
                           (list :id-ref "'count'")))
               'spirv-core:bit-reverse
               (list :value 204 :required-capabilities (list :shader) :resultp
                     :typed :operands (list (list :id-ref "'base'")))
               'spirv-core:bit-count
               (list :value 205 :required-capabilities nil :resultp :typed
                     :operands (list (list :id-ref "'base'")))
               'spirv-core:dp-dx
               (list :value 207 :required-capabilities (list :shader) :resultp
                     :typed :operands (list (list :id-ref "'p'")))
               'spirv-core:dp-dy
               (list :value 208 :required-capabilities (list :shader) :resultp
                     :typed :operands (list (list :id-ref "'p'")))
               'spirv-core:fwidth
               (list :value 209 :required-capabilities (list :shader) :resultp
                     :typed :operands (list (list :id-ref "'p'")))
               'spirv-core:dp-dx-fine
               (list :value 210 :required-capabilities
                     (list :derivative-control) :resultp :typed :operands
                     (list (list :id-ref "'p'")))
               'spirv-core:dp-dy-fine
               (list :value 211 :required-capabilities
                     (list :derivative-control) :resultp :typed :operands
                     (list (list :id-ref "'p'")))
               'spirv-core:fwidth-fine
               (list :value 212 :required-capabilities
                     (list :derivative-control) :resultp :typed :operands
                     (list (list :id-ref "'p'")))
               'spirv-core:dp-dx-coarse
               (list :value 213 :required-capabilities
                     (list :derivative-control) :resultp :typed :operands
                     (list (list :id-ref "'p'")))
               'spirv-core:dp-dy-coarse
               (list :value 214 :required-capabilities
                     (list :derivative-control) :resultp :typed :operands
                     (list (list :id-ref "'p'")))
               'spirv-core:fwidth-coarse
               (list :value 215 :required-capabilities
                     (list :derivative-control) :resultp :typed :operands
                     (list (list :id-ref "'p'")))
               'spirv-core:emit-vertex
               (list :value 218 :required-capabilities (list :geometry)
                     :operands nil)
               'spirv-core:end-primitive
               (list :value 219 :required-capabilities (list :geometry)
                     :operands nil)
               'spirv-core:emit-stream-vertex
               (list :value 220 :required-capabilities
                     (list :geometry-streams) :operands
                     (list (list :id-ref "'stream'")))
               'spirv-core:end-stream-primitive
               (list :value 221 :required-capabilities
                     (list :geometry-streams) :operands
                     (list (list :id-ref "'stream'")))
               'spirv-core:control-barrier
               (list :value 224 :required-capabilities nil :operands
                     (list (list :id-scope "'execution'")
                           (list :id-scope "'memory'")
                           (list :id-memory-semantics "'semantics'")))
               'spirv-core:memory-barrier
               (list :value 225 :required-capabilities nil :operands
                     (list (list :id-scope "'memory'")
                           (list :id-memory-semantics "'semantics'")))
               'spirv-core:atomic-load
               (list :value 227 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")))
               'spirv-core:atomic-store
               (list :value 228 :required-capabilities nil :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:atomic-exchange
               (list :value 229 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:atomic-compare-exchange
               (list :value 230 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'equal'")
                           (list :id-memory-semantics "'unequal'")
                           (list :id-ref "'value'") (list :id-ref "'comparator'")))
               'spirv-core:atomic-compare-exchange-weak
               (list :value 231 :required-capabilities (list :kernel) :resultp
                     :typed :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'equal'")
                           (list :id-memory-semantics "'unequal'")
                           (list :id-ref "'value'") (list :id-ref "'comparator'")))
               'spirv-core:atomic-i-increment
               (list :value 232 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")))
               'spirv-core:atomic-i-decrement
               (list :value 233 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")))
               'spirv-core:atomic-i-add
               (list :value 234 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:atomic-i-sub
               (list :value 235 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:atomic-s-min
               (list :value 236 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:atomic-u-min
               (list :value 237 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:atomic-s-max
               (list :value 238 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:atomic-u-max
               (list :value 239 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:atomic-and
               (list :value 240 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:atomic-or
               (list :value 241 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:atomic-xor
               (list :value 242 :required-capabilities nil :resultp :typed
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")
                           (list :id-ref "'value'")))
               'spirv-core:phi
               (list :value 245 :required-capabilities nil :resultp :typed
                     :operands
                     (list
                      (list (list :* :pair-id-ref-id-ref)
                            "'variable, parent, ...'")))
               'spirv-core:loop-merge
               (list :value 246 :required-capabilities nil :operands
                     (list (list :id-ref "'merge block'")
                           (list :id-ref "'continue target'") (list :loop-control)))
               'spirv-core:selection-merge
               (list :value 247 :required-capabilities nil :operands
                     (list (list :id-ref "'merge block'")
                           (list :selection-control)))
               'spirv-core:label
               (list :value 248 :required-capabilities nil :resultp t
                     :operands nil)
               'spirv-core:branch
               (list :value 249 :required-capabilities nil :operands
                     (list (list :id-ref "'target label'")))
               'spirv-core:branch-conditional
               (list :value 250 :required-capabilities nil :operands
                     (list (list :id-ref "'condition'")
                           (list :id-ref "'true label'") (list :id-ref "'false label'")
                           (list (list :* :literal-integer) "'branch weights'")))
               'spirv-core:switch
               (list :value 251 :required-capabilities nil :operands
                     (list (list :id-ref "'selector'") (list :id-ref "'default'")
                           (list (list :* :pair-literal-integer-id-ref) "'target'")))
               'spirv-core:kill
               (list :value 252 :required-capabilities (list :shader)
                     :operands nil)
               'spirv-core:return
               (list :value 253 :required-capabilities nil :operands nil)
               'spirv-core:return-value
               (list :value 254 :required-capabilities nil :operands
                     (list (list :id-ref "'value'")))
               'spirv-core:unreachable
               (list :value 255 :required-capabilities nil :operands nil)
               'spirv-core:lifetime-start
               (list :value 256 :required-capabilities (list :kernel)
                     :operands
                     (list (list :id-ref "'pointer'")
                           (list :literal-integer "'size'")))
               'spirv-core:lifetime-stop
               (list :value 257 :required-capabilities (list :kernel)
                     :operands
                     (list (list :id-ref "'pointer'")
                           (list :literal-integer "'size'")))
               'spirv-core:group-async-copy
               (list :value 259 :required-capabilities (list :kernel) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :id-ref "'destination'") (list :id-ref "'source'")
                           (list :id-ref "'num elements'") (list :id-ref "'stride'")
                           (list :id-ref "'event'")))
               'spirv-core:group-wait-events
               (list :value 260 :required-capabilities (list :kernel)
                     :operands
                     (list (list :id-scope "'execution'")
                           (list :id-ref "'num events'") (list :id-ref "'events list'")))
               'spirv-core:group-all
               (list :value 261 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :id-ref "'predicate'")))
               'spirv-core:group-any
               (list :value 262 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :id-ref "'predicate'")))
               'spirv-core:group-broadcast
               (list :value 263 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'") (list :id-ref "'value'")
                           (list :id-ref "'localid'")))
               'spirv-core:group-i-add
               (list :value 264 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :group-operation "'operation'") (list :id-ref "'x'")))
               'spirv-core:group-f-add
               (list :value 265 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :group-operation "'operation'") (list :id-ref "'x'")))
               'spirv-core:group-f-min
               (list :value 266 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :group-operation "'operation'") (list :id-ref "x")))
               'spirv-core:group-u-min
               (list :value 267 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :group-operation "'operation'") (list :id-ref "'x'")))
               'spirv-core:group-s-min
               (list :value 268 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :group-operation "'operation'") (list :id-ref "x")))
               'spirv-core:group-f-max
               (list :value 269 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :group-operation "'operation'") (list :id-ref "x")))
               'spirv-core:group-u-max
               (list :value 270 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :group-operation "'operation'") (list :id-ref "x")))
               'spirv-core:group-s-max
               (list :value 271 :required-capabilities (list :groups) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'")
                           (list :group-operation "'operation'") (list :id-ref "x")))
               'spirv-core:read-pipe
               (list :value 274 :required-capabilities (list :pipes) :resultp
                     :typed :operands
                     (list (list :id-ref "'pipe'") (list :id-ref "'pointer'")
                           (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:write-pipe
               (list :value 275 :required-capabilities (list :pipes) :resultp
                     :typed :operands
                     (list (list :id-ref "'pipe'") (list :id-ref "'pointer'")
                           (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:reserved-read-pipe
               (list :value 276 :required-capabilities (list :pipes) :resultp
                     :typed :operands
                     (list (list :id-ref "'pipe'") (list :id-ref "'reserve id'")
                           (list :id-ref "'index'") (list :id-ref "'pointer'")
                           (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:reserved-write-pipe
               (list :value 277 :required-capabilities (list :pipes) :resultp
                     :typed :operands
                     (list (list :id-ref "'pipe'") (list :id-ref "'reserve id'")
                           (list :id-ref "'index'") (list :id-ref "'pointer'")
                           (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:reserve-read-pipe-packets
               (list :value 278 :required-capabilities (list :pipes) :resultp
                     :typed :operands
                     (list (list :id-ref "'pipe'") (list :id-ref "'num packets'")
                           (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:reserve-write-pipe-packets
               (list :value 279 :required-capabilities (list :pipes) :resultp
                     :typed :operands
                     (list (list :id-ref "'pipe'") (list :id-ref "'num packets'")
                           (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:commit-read-pipe
               (list :value 280 :required-capabilities (list :pipes) :operands
                     (list (list :id-ref "'pipe'") (list :id-ref "'reserve id'")
                           (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:commit-write-pipe
               (list :value 281 :required-capabilities (list :pipes) :operands
                     (list (list :id-ref "'pipe'") (list :id-ref "'reserve id'")
                           (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:is-valid-reserve-id
               (list :value 282 :required-capabilities (list :pipes) :resultp
                     :typed :operands (list (list :id-ref "'reserve id'")))
               'spirv-core:get-num-pipe-packets
               (list :value 283 :required-capabilities (list :pipes) :resultp
                     :typed :operands
                     (list (list :id-ref "'pipe'") (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:get-max-pipe-packets
               (list :value 284 :required-capabilities (list :pipes) :resultp
                     :typed :operands
                     (list (list :id-ref "'pipe'") (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:group-reserve-read-pipe-packets
               (list :value 285 :required-capabilities (list :pipes) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'") (list :id-ref "'pipe'")
                           (list :id-ref "'num packets'") (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:group-reserve-write-pipe-packets
               (list :value 286 :required-capabilities (list :pipes) :resultp
                     :typed :operands
                     (list (list :id-scope "'execution'") (list :id-ref "'pipe'")
                           (list :id-ref "'num packets'") (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:group-commit-read-pipe
               (list :value 287 :required-capabilities (list :pipes) :operands
                     (list (list :id-scope "'execution'") (list :id-ref "'pipe'")
                           (list :id-ref "'reserve id'") (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:group-commit-write-pipe
               (list :value 288 :required-capabilities (list :pipes) :operands
                     (list (list :id-scope "'execution'") (list :id-ref "'pipe'")
                           (list :id-ref "'reserve id'") (list :id-ref "'packet size'")
                           (list :id-ref "'packet alignment'")))
               'spirv-core:enqueue-marker
               (list :value 291 :required-capabilities (list :device-enqueue)
                     :resultp :typed :operands
                     (list (list :id-ref "'queue'") (list :id-ref "'num events'")
                           (list :id-ref "'wait events'") (list :id-ref "'ret event'")))
               'spirv-core:enqueue-kernel
               (list :value 292 :required-capabilities (list :device-enqueue)
                     :resultp :typed :operands
                     (list (list :id-ref "'queue'") (list :id-ref "'flags'")
                           (list :id-ref "'nd range'") (list :id-ref "'num events'")
                           (list :id-ref "'wait events'") (list :id-ref "'ret event'")
                           (list :id-ref "'invoke'") (list :id-ref "'param'")
                           (list :id-ref "'param size'") (list :id-ref "'param align'")
                           (list (list :* :id-ref) "'local size'")))
               'spirv-core:get-kernel-nd-range-sub-group-count
               (list :value 293 :required-capabilities (list :device-enqueue)
                     :resultp :typed :operands
                     (list (list :id-ref "'nd range'") (list :id-ref "'invoke'")
                           (list :id-ref "'param'") (list :id-ref "'param size'")
                           (list :id-ref "'param align'")))
               'spirv-core:get-kernel-nd-range-max-sub-group-size
               (list :value 294 :required-capabilities (list :device-enqueue)
                     :resultp :typed :operands
                     (list (list :id-ref "'nd range'") (list :id-ref "'invoke'")
                           (list :id-ref "'param'") (list :id-ref "'param size'")
                           (list :id-ref "'param align'")))
               'spirv-core:get-kernel-work-group-size
               (list :value 295 :required-capabilities (list :device-enqueue)
                     :resultp :typed :operands
                     (list (list :id-ref "'invoke'") (list :id-ref "'param'")
                           (list :id-ref "'param size'") (list :id-ref "'param align'")))
               'spirv-core:get-kernel-preferred-work-group-size-multiple
               (list :value 296 :required-capabilities (list :device-enqueue)
                     :resultp :typed :operands
                     (list (list :id-ref "'invoke'") (list :id-ref "'param'")
                           (list :id-ref "'param size'") (list :id-ref "'param align'")))
               'spirv-core:retain-event
               (list :value 297 :required-capabilities (list :device-enqueue)
                     :operands (list (list :id-ref "'event'")))
               'spirv-core:release-event
               (list :value 298 :required-capabilities (list :device-enqueue)
                     :operands (list (list :id-ref "'event'")))
               'spirv-core:create-user-event
               (list :value 299 :required-capabilities (list :device-enqueue)
                     :resultp :typed :operands nil)
               'spirv-core:is-valid-event
               (list :value 300 :required-capabilities (list :device-enqueue)
                     :resultp :typed :operands (list (list :id-ref "'event'")))
               'spirv-core:set-user-event-status
               (list :value 301 :required-capabilities (list :device-enqueue)
                     :operands
                     (list (list :id-ref "'event'") (list :id-ref "'status'")))
               'spirv-core:capture-event-profiling-info
               (list :value 302 :required-capabilities (list :device-enqueue)
                     :operands
                     (list (list :id-ref "'event'")
                           (list :id-ref "'profiling info'") (list :id-ref "'value'")))
               'spirv-core:get-default-queue
               (list :value 303 :required-capabilities (list :device-enqueue)
                     :resultp :typed :operands nil)
               'spirv-core:build-nd-range
               (list :value 304 :required-capabilities (list :device-enqueue)
                     :resultp :typed :operands
                     (list (list :id-ref "'globalworksize'")
                           (list :id-ref "'localworksize'")
                           (list :id-ref "'globalworkoffset'")))
               'spirv-core:image-sparse-sample-implicit-lod
               (list :value 305 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sparse-sample-explicit-lod
               (list :value 306 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :image-operands)))
               'spirv-core:image-sparse-sample-dref-implicit-lod
               (list :value 307 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'d~ref~'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sparse-sample-dref-explicit-lod
               (list :value 308 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'d~ref~'")
                           (list :image-operands)))
               'spirv-core:image-sparse-sample-proj-implicit-lod
               (list :value 309 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sparse-sample-proj-explicit-lod
               (list :value 310 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :image-operands)))
               'spirv-core:image-sparse-sample-proj-dref-implicit-lod
               (list :value 311 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'d~ref~'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sparse-sample-proj-dref-explicit-lod
               (list :value 312 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'d~ref~'")
                           (list :image-operands)))
               'spirv-core:image-sparse-fetch
               (list :value 313 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'image'") (list :id-ref "'coordinate'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sparse-gather
               (list :value 314 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'component'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sparse-dref-gather
               (list :value 315 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'sampled image'")
                           (list :id-ref "'coordinate'") (list :id-ref "'d~ref~'")
                           (list (list :? :image-operands))))
               'spirv-core:image-sparse-texels-resident
               (list :value 316 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'resident code'")))
               'spirv-core:no-line
               (list :value 317 :required-capabilities nil :operands nil)
               'spirv-core:atomic-flag-test-and-set
               (list :value 318 :required-capabilities (list :kernel) :resultp
                     :typed :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")))
               'spirv-core:atomic-flag-clear
               (list :value 319 :required-capabilities (list :kernel)
                     :operands
                     (list (list :id-ref "'pointer'") (list :id-scope "'scope'")
                           (list :id-memory-semantics "'semantics'")))
               'spirv-core:image-sparse-read
               (list :value 320 :required-capabilities
                     (list :sparse-residency) :resultp :typed :operands
                     (list (list :id-ref "'image'") (list :id-ref "'coordinate'")
                           (list (list :? :image-operands))))
               'spirv-core:size-of
               (list :value 321 :required-capabilities (list :addresses)
                     :resultp :typed :operands (list (list :id-ref "'pointer'")))
               'spirv-core:type-pipe-storage
               (list :value 322 :required-capabilities (list :pipe-storage)
                     :resultp t :operands nil)
               'spirv-core:constant-pipe-storage
               (list :value 323 :required-capabilities (list :pipe-storage)
                     :resultp :typed :operands
                     (list (list :literal-integer "'packet size'")
                           (list :literal-integer "'packet alignment'")
                           (list :literal-integer "'capacity'")))
               'spirv-core:create-pipe-from-pipe-storage
               (list :value 324 :required-capabilities (list :pipe-storage)
                     :resultp :typed :operands
                     (list (list :id-ref "'pipe storage'")))
               'spirv-core:get-kernel-local-size-for-subgroup-count
               (list :value 325 :required-capabilities
                     (list :subgroup-dispatch) :resultp :typed :operands
                     (list (list :id-ref "'subgroup count'")
                           (list :id-ref "'invoke'") (list :id-ref "'param'")
                           (list :id-ref "'param size'") (list :id-ref "'param align'")))
               'spirv-core:get-kernel-max-num-subgroups
               (list :value 326 :required-capabilities
                     (list :subgroup-dispatch) :resultp :typed :operands
                     (list (list :id-ref "'invoke'") (list :id-ref "'param'")
                           (list :id-ref "'param size'") (list :id-ref "'param align'")))
               'spirv-core:type-named-barrier
               (list :value 327 :required-capabilities (list :named-barrier)
                     :resultp t :operands nil)
               'spirv-core:named-barrier-initialize
               (list :value 328 :required-capabilities (list :named-barrier)
                     :resultp :typed :operands
                     (list (list :id-ref "'subgroup count'")))
               'spirv-core:memory-named-barrier
               (list :value 329 :required-capabilities (list :named-barrier)
                     :operands
                     (list (list :id-ref "'named barrier'")
                           (list :id-scope "'memory'")
                           (list :id-memory-semantics "'semantics'")))
               'spirv-core:module-processed
               (list :value 330 :required-capabilities nil :operands
                     (list (list :literal-string "'process'")))
               'spirv-core:subgroup-ballot-khr
               (list :value 4421 :required-capabilities
                     (list :subgroup-ballot-khr) :resultp :typed :operands
                     (list (list :id-ref "'predicate'")))
               'spirv-core:subgroup-first-invocation-khr
               (list :value 4422 :required-capabilities
                     (list :subgroup-ballot-khr) :resultp :typed :operands
                     (list (list :id-ref "'value'")))))

        :glsl-opcodes
        (alexandria:plist-hash-table
         (list 'spirv-glsl450:round
               (list :value 1 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:round-even
               (list :value 2 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:trunc
               (list :value 3 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:f-abs
               (list :value 4 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:s-abs
               (list :value 5 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:f-sign
               (list :value 6 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:s-sign
               (list :value 7 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:floor
               (list :value 8 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:ceil
               (list :value 9 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:fract
               (list :value 10 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:radians
               (list :value 11 :required-capabilities nil :operands
                     (list (list :id-ref "'degrees'")))
               'spirv-glsl450:degrees
               (list :value 12 :required-capabilities nil :operands
                     (list (list :id-ref "'radians'")))
               'spirv-glsl450:sin
               (list :value 13 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:cos
               (list :value 14 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:tan
               (list :value 15 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:asin
               (list :value 16 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:acos
               (list :value 17 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:atan
               (list :value 18 :required-capabilities nil :operands
                     (list (list :id-ref "'y_over_x'")))
               'spirv-glsl450:sinh
               (list :value 19 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:cosh
               (list :value 20 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:tanh
               (list :value 21 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:asinh
               (list :value 22 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:acosh
               (list :value 23 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:atanh
               (list :value 24 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:atan2
               (list :value 25 :required-capabilities nil :operands
                     (list (list :id-ref "'y'") (list :id-ref "'x'")))
               'spirv-glsl450:pow
               (list :value 26 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-glsl450:exp
               (list :value 27 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:log
               (list :value 28 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:exp2
               (list :value 29 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:log2
               (list :value 30 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:sqrt
               (list :value 31 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:inverse-sqrt
               (list :value 32 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:determinant
               (list :value 33 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:matrix-inverse
               (list :value 34 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:modf
               (list :value 35 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'i'")))
               'spirv-glsl450:modf-struct
               (list :value 36 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:f-min
               (list :value 37 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-glsl450:u-min
               (list :value 38 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-glsl450:s-min
               (list :value 39 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-glsl450:f-max
               (list :value 40 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-glsl450:u-max
               (list :value 41 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-glsl450:s-max
               (list :value 42 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-glsl450:f-clamp
               (list :value 43 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'minval'")
                           (list :id-ref "'maxval'")))
               'spirv-glsl450:u-clamp
               (list :value 44 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'minval'")
                           (list :id-ref "'maxval'")))
               'spirv-glsl450:s-clamp
               (list :value 45 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'minval'")
                           (list :id-ref "'maxval'")))
               'spirv-glsl450:f-mix
               (list :value 46 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")
                           (list :id-ref "'a'")))
               'spirv-glsl450:i-mix
               (list :value 47 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")
                           (list :id-ref "'a'")))
               'spirv-glsl450:step
               (list :value 48 :required-capabilities nil :operands
                     (list (list :id-ref "'edge'") (list :id-ref "'x'")))
               'spirv-glsl450:smooth-step
               (list :value 49 :required-capabilities nil :operands
                     (list (list :id-ref "'edge0'") (list :id-ref "'edge1'")
                           (list :id-ref "'x'")))
               'spirv-glsl450:fma
               (list :value 50 :required-capabilities nil :operands
                     (list (list :id-ref "'a'") (list :id-ref "'b'")
                           (list :id-ref "'c'")))
               'spirv-glsl450:frexp
               (list :value 51 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'exp'")))
               'spirv-glsl450:frexp-struct
               (list :value 52 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:ldexp
               (list :value 53 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'exp'")))
               'spirv-glsl450:pack-snorm-4x8
               (list :value 54 :required-capabilities nil :operands
                     (list (list :id-ref "'v'")))
               'spirv-glsl450:pack-unorm-4x8
               (list :value 55 :required-capabilities nil :operands
                     (list (list :id-ref "'v'")))
               'spirv-glsl450:pack-snorm-2x16
               (list :value 56 :required-capabilities nil :operands
                     (list (list :id-ref "'v'")))
               'spirv-glsl450:pack-unorm-2x16
               (list :value 57 :required-capabilities nil :operands
                     (list (list :id-ref "'v'")))
               'spirv-glsl450:pack-half-2x16
               (list :value 58 :required-capabilities nil :operands
                     (list (list :id-ref "'v'")))
               'spirv-glsl450:pack-double-2x32
               (list :value 59 :required-capabilities (list :float64)
                     :operands (list (list :id-ref "'v'")))
               'spirv-glsl450:unpack-snorm-2x16
               (list :value 60 :required-capabilities nil :operands
                     (list (list :id-ref "'p'")))
               'spirv-glsl450:unpack-unorm-2x16
               (list :value 61 :required-capabilities nil :operands
                     (list (list :id-ref "'p'")))
               'spirv-glsl450:unpack-half-2x16
               (list :value 62 :required-capabilities nil :operands
                     (list (list :id-ref "'v'")))
               'spirv-glsl450:unpack-snorm-4x8
               (list :value 63 :required-capabilities nil :operands
                     (list (list :id-ref "'p'")))
               'spirv-glsl450:unpack-unorm-4x8
               (list :value 64 :required-capabilities nil :operands
                     (list (list :id-ref "'p'")))
               'spirv-glsl450:unpack-double-2x32
               (list :value 65 :required-capabilities (list :float64)
                     :operands (list (list :id-ref "'v'")))
               'spirv-glsl450:length
               (list :value 66 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:distance
               (list :value 67 :required-capabilities nil :operands
                     (list (list :id-ref "'p0'") (list :id-ref "'p1'")))
               'spirv-glsl450:cross
               (list :value 68 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-glsl450:normalize
               (list :value 69 :required-capabilities nil :operands
                     (list (list :id-ref "'x'")))
               'spirv-glsl450:face-forward
               (list :value 70 :required-capabilities nil :operands
                     (list (list :id-ref "'n'") (list :id-ref "'i'")
                           (list :id-ref "'nref'")))
               'spirv-glsl450:reflect
               (list :value 71 :required-capabilities nil :operands
                     (list (list :id-ref "'i'") (list :id-ref "'n'")))
               'spirv-glsl450:refract
               (list :value 72 :required-capabilities nil :operands
                     (list (list :id-ref "'i'") (list :id-ref "'n'")
                           (list :id-ref "'eta'")))
               'spirv-glsl450:find-i-lsb
               (list :value 73 :required-capabilities nil :operands
                     (list (list :id-ref "'value'")))
               'spirv-glsl450:find-s-msb
               (list :value 74 :required-capabilities nil :operands
                     (list (list :id-ref "'value'")))
               'spirv-glsl450:find-u-msb
               (list :value 75 :required-capabilities nil :operands
                     (list (list :id-ref "'value'")))
               'spirv-glsl450:interpolate-at-centroid
               (list :value 76 :required-capabilities
                     (list :interpolation-function) :operands
                     (list (list :id-ref "'interpolant'")))
               'spirv-glsl450:interpolate-at-sample
               (list :value 77 :required-capabilities
                     (list :interpolation-function) :operands
                     (list (list :id-ref "'interpolant'")
                           (list :id-ref "'sample'")))
               'spirv-glsl450:interpolate-at-offset
               (list :value 78 :required-capabilities
                     (list :interpolation-function) :operands
                     (list (list :id-ref "'interpolant'")
                           (list :id-ref "'offset'")))
               'spirv-glsl450:n-min
               (list :value 79 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-glsl450:n-max
               (list :value 80 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'y'")))
               'spirv-glsl450:n-clamp
               (list :value 81 :required-capabilities nil :operands
                     (list (list :id-ref "'x'") (list :id-ref "'minval'")
                           (list :id-ref "'maxval'")))))))
