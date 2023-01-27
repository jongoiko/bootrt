#!/bin/python3

#   bootrt.py
# Script to emulate and debug the raytracer's asm version

from sys import stderr

FRAC_BITS = 7

def FP_LIT(n):
    return n << FRAC_BITS

IMG_HEIGHT = 200
IMG_WIDTH = 320

CAMERA = [0, 0, 0]

VIEWPORT_HEIGHT = FP_LIT(50)
VIEWPORT_WIDTH = FP_LIT(80)

FOCAL_LENGTH = FP_LIT(30)

SPHERE_RADIUS = FP_LIT(40)
SPHERE_CENTER = [CAMERA[0], CAMERA[1], FOCAL_LENGTH + SPHERE_RADIUS]

VIEWPORT_UPPER_LEFT = [CAMERA[0] - VIEWPORT_WIDTH // 2, CAMERA[1] +
                       VIEWPORT_HEIGHT // 2, FOCAL_LENGTH]

def FP_TO_INT(n):
    return n >> FRAC_BITS

def eprint(*args, **kwargs):
    print(*args, file=stderr, **kwargs)

def dot(u, v):
    return sum([FP_TO_INT(a * b) for a, b in zip(u, v)])

def get_color(row, col):
    VP_UPPER_LEFT = VIEWPORT_UPPER_LEFT.copy()
    VP_UPPER_LEFT[0] += (col // (IMG_WIDTH - 1)) * FP_TO_INT(VIEWPORT_WIDTH)
    VP_UPPER_LEFT[1] -= (row // (IMG_HEIGHT - 1)) * FP_TO_INT(VIEWPORT_HEIGHT)
    direction = [a - b for a, b in zip(VP_UPPER_LEFT, CAMERA)]
    OC = [a - b for a, b in zip(SPHERE_CENTER, CAMERA)]
    B = 2 * dot(OC, direction)
    A = dot(direction, direction)
    C = dot(OC, OC) - FP_TO_INT(SPHERE_RADIUS ** 2)
    A = A >> 8
    B = B >> 8
    C = C >> 8
    A1, A2 = FP_TO_INT(B ** 2), FP_TO_INT(4 * A * C)
    return [0, 0, 255 if A1 > A2 else 0]

def main():
    print("P3\n{} {}\n255".format(IMG_WIDTH, IMG_HEIGHT))
    for row in range(0, FP_LIT(IMG_HEIGHT), FP_LIT(1)):
        for col in range(0, FP_LIT(IMG_WIDTH), FP_LIT(1)):
            color = get_color(row, col)
            print(color[0], color[1], color[2])

if __name__ == "__main__":
    main()
