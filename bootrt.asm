;##############################################################################
;
;   bootrt - A simple raytracer in a 512-byte boot sector
;
; Compile to a raw binary with 'nasm -f bin bootrt.asm -o bootrt.bin'
;
;##############################################################################

    bits    16
    org     0x7C00
    cpu     8086

VGA_MODE:           equ     0x13
SECTOR_SIZE:        equ     512
VIDEO_MEM_SEGMENT:  equ     0xA000

IMG_HEIGHT:         equ     200
IMG_WIDTH:          equ     320
IMG_SIZE:           equ     (IMG_WIDTH * IMG_HEIGHT)

; Each 3D vector is composed of three 16-bit fixed-point numbers
VECTOR_SIZE:        equ     6

; Scalars in vectors are represented in (signed) fixed-point format, where the
; lower FRAC_BITS bits hold the fractional part and the rest hold the integer
; part
FRAC_BITS:          equ     7

; Macro for fixed-points literals
%define     FP_LIT(int, frac)   ((int) << FRAC_BITS + (frac))

; Origin/Position of the camera
CAMERA_X:           equ     0
CAMERA_Y:           equ     0
CAMERA_Z:           equ     0

; Distance from camera position to the viewport along the Z dimension
FOCAL_LENGTH:       equ     FP_LIT(30, 0)

; VGA mode 13 has a 320:200 = 8:5 aspect ratio, so we preserve it in the
; viewport's dimensions
VIEWPORT_HEIGHT:    equ     FP_LIT(50, 0)
VIEWPORT_WIDTH:     equ     FP_LIT(80, 0)

VIEWPORT_UPPER_LEFT_X:  equ     (CAMERA_X - VIEWPORT_WIDTH / 2)
VIEWPORT_UPPER_LEFT_Y:  equ     (CAMERA_Y + VIEWPORT_HEIGHT / 2)
VIEWPORT_UPPER_LEFT_Z:  equ     FOCAL_LENGTH

SPHERE_RADIUS:      equ     FP_LIT(40, 0)

SPHERE_CENTER_X:    equ     CAMERA_X
SPHERE_CENTER_Y:    equ     CAMERA_Y
SPHERE_CENTER_Z:    equ     (FOCAL_LENGTH + SPHERE_RADIUS)

SPHERE_COLOR:       equ     0x28
BACKGROUND_COLOR:   equ     0x11

%macro store_vector 4
    mov     di, IMG_SIZE + %1 * VECTOR_SIZE
    mov     word es:[di], %2
    mov     word es:[di + 2], %3
    mov     word es:[di + 4], %4
%endmacro

start:
    ; Set VGA video mode to 320x200 256-color (mode 13h)
    mov     ax, VGA_MODE
    int     0x10

    ; Load video memory segment (physical address 0xA000)
    mov     ax, VIDEO_MEM_SEGMENT
    mov     es, ax

    ; Store origin and the sphere's center
    store_vector 0, CAMERA_X, CAMERA_Y, CAMERA_Z
    store_vector 2, SPHERE_CENTER_X, SPHERE_CENTER_Y, SPHERE_CENTER_Z

    ; Vector 2 <- OC: vector from the origin to the sphere's center
    mov     ax, 0x0200
    call    vector_sub

    ; Row and column numbers are stored in fixed-point format and only
    ; converted to integers to calculate pixel addresses
    xor     di, di  ; DI <- row number
loop_rows:
    cmp     di, FP_LIT(IMG_HEIGHT, 0)
    jae     end
    xor     si, si  ; SI <- column number
loop_columns:
    cmp     si, FP_LIT(IMG_WIDTH, 0)
    jae     next_row

    push    di
    ; Store viewport upper left corner vector
    store_vector 1, VIEWPORT_UPPER_LEFT_X, VIEWPORT_UPPER_LEFT_Y, VIEWPORT_UPPER_LEFT_Z
    pop     di

    ; Translate pixel column to horizontal component relative to the viewport's
    ; upper left corner
    xor     dx, dx
    mov     ax, si
    mov     cx, IMG_WIDTH - 1
    idiv    cx
    mov     cx, VIEWPORT_WIDTH >> FRAC_BITS
    imul    cx
    add     es:[0xFA06], ax

    ; Translate pixel row to vertical component relative to the viewport's
    ; upper left corner
    xor     dx, dx
    mov     ax, di
    mov     cx, IMG_HEIGHT - 1
    idiv    cx
    mov     cx, VIEWPORT_HEIGHT >> FRAC_BITS
    imul    cx
    sub     es:[0xFA08], ax

    ; Subtract origin from resulting vector to obtain the ray's direction (b)
    mov     ax, 0x0100
    call    vector_sub

    ; Obtain color intersecting with the outgoing ray. The ray can be
    ; represented as
    ;   P(t) = Origin + t * Direction
    ; The ray's origin is stored in vector 0 and its direction vector is stored
    ; in vector 1

    ; A, B and C are shifted to the right by 8 bits to prevent later
    ; calculations from resulting in very large numbers (and thus saving the
    ; need to deal with 32-bit multiplication)

    ; AX <- B = 2 * dot(OC, b)
    mov     ax, 0x0201
    call    vector_dot
    shl     dx, 1
    shl     ax, 1
    adc     dx, 0
    mov     al, ah
    mov     ah, dl
    push    ax

    ; AX <- A = b^2
    mov     ax, 0x0101
    call    vector_dot
    mov     al, ah
    mov     ah, dl
    push    ax

    ; AX <- C = dot(OC, OC) - radius^2
    mov     ax, 0x0202
    call    vector_dot
    sub     ax, ((SPHERE_RADIUS * SPHERE_RADIUS) >> FRAC_BITS) & 0xFFFF
    sbb     dx, (((SPHERE_RADIUS * SPHERE_RADIUS) >> FRAC_BITS) & 0xFFFF0000) >> 16
    mov     al, ah
    mov     ah, dl

    ; BP:BX <- 4 * A * C; A and C have each been multiplied by 2 already
    shl     ax, 1
    pop     cx
    shl     cx, 1
    imul    cx
    call    rescale_dx_ax
    mov     bp, dx
    mov     bx, ax

    ; DX:AX <- B^2
    pop     cx
    mov     ax, cx
    imul    cx
    call    rescale_dx_ax

    ; The ray intersects the sphere if and only if B^2 > 4AC (DX:AX > BP:BX)
    cmp     dx, bp
    jg      hit_sphere
    jl      no_hit
    cmp     ax, bx
    ja      hit_sphere
no_hit:
    mov     bp, BACKGROUND_COLOR
    jmp     draw_pixel
hit_sphere:
    mov     bp, SPHERE_COLOR
draw_pixel:
    ; Draw pixel at row, column (DI, SI): convert the values of both registers
    ; to integers and calculate the pixel's address
    mov     ax, di
    mov     cl, FRAC_BITS
    shr     ax, cl
    mov     bx, IMG_WIDTH
    mul     bx
    mov     bx, si
    shr     bx, cl
    add     bx, ax

    mov     dx, bp
    mov     byte es:[bx], dl

    add     si, FP_LIT(1, 0)
    jmp     loop_columns
next_row:
    add     di, FP_LIT(1, 0)
    jmp     loop_rows

end:
    jmp     end

;   get_vector_addresses
; Loads to BP, BX the addresses of vectors at AH and AL respectively (relative
; to ES). Destroys the values of AX, CX and DX
get_vector_addresses:
    mov     cx, ax
    mov     ax, VECTOR_SIZE
    xor     bx, bx
    mov     bl, cl
    mul     bx
    add     ax, IMG_SIZE
    push    ax
    mov     ax, VECTOR_SIZE
    mov     bl, ch
    mul     bx
    add     ax, IMG_SIZE
    mov     bp, ax
    pop     bx
    ret

;   vector_add
; Adds vector AL to vector AH. Destroys the values of AX, BX, CX and BP
vector_add:
    call    get_vector_addresses
    mov     cx, word es:[bx]
    add     es:[bp], cx
    mov     cx, word es:[bx + 2]
    add     es:[bp + 2], cx
    mov     cx, word es:[bx + 4]
    add     es:[bp + 4], cx
    ret

;   vector_sub
; Subtracts vector AL from vector AH. Destroys the values of AX, BX, CX and BP
vector_sub:
    call    get_vector_addresses
    mov     cx, word es:[bx]
    sub     es:[bp], cx
    mov     cx, word es:[bx + 2]
    sub     es:[bp + 2], cx
    mov     cx, word es:[bx + 4]
    sub     es:[bp + 4], cx
    ret

;   vector_dot
; Loads to DX:AX the dot product of vectors AL and AH. Destroys the values of
; BX, CX and BP
vector_dot:
    call    get_vector_addresses
    mov     cx, word es:[bx]
    mov     ax, word es:[bp]
    imul    cx
    call    rescale_dx_ax
    push    dx
    push    ax

    mov     cx, word es:[bx + 2]
    mov     ax, word es:[bp + 2]
    imul    cx
    call    rescale_dx_ax
    push    dx
    push    ax

    mov     cx, word es:[bx + 4]
    mov     ax, word es:[bp + 4]
    imul    cx
    call    rescale_dx_ax
    pop     cx
    add     ax, cx
    pop     cx
    adc     dx, cx
    pop     cx
    add     ax, cx
    pop     cx
    adc     dx, cx
    ret

;   rescale_dx_ax
; Re-scales the value in DX:AX to the fixed-point format. Destroys the value in
; CX
rescale_dx_ax:
    mov     cl, FRAC_BITS
    shr     ax, cl
    mov     cl, 16 - FRAC_BITS
    push    dx
    shl     dx, cl
    or      ax, dx
    pop     dx
    mov     cl, FRAC_BITS
    sar     dx, cl
    ret

    ; Fill up rest of sector with zeros until boot sector signature
    times   SECTOR_SIZE - 2 - ($ - $$) db 0
    dw      0xAA55
