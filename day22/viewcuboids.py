#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np

def read_cuboids(filename):
    cuboids = []
    with open(filename, 'r') as file:
        for line in file:
            parts = line.strip().split(',')
            if len(parts) == 6:
                x, y, z, w, h, d = map(int, parts)
                cuboids.append((x, y, z, w, h, d))
    return cuboids

def get_axes_limits(cuboids):
    all_x = [x + w for x, y, z, w, h, d in cuboids]
    all_y = [y + h for x, y, z, w, h, d in cuboids]
    all_z = [z + d for x, y, z, w, h, d in cuboids]

    max_val = max(max(all_x), max(all_y), max(all_z))

    return max_val


def main():
    cuboids = read_cuboids("cuboids.txt")
    amax = get_axes_limits(cuboids)

    cmap = plt.colormaps['tab20']

    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')

    for i, cuboid in enumerate(cuboids):
        x, y, z, w, h, d = cuboid
        color = cmap(i % 20)
        voxel = np.zeros((x + w, y + h, z + d), dtype=bool)
        voxel[x:x + w, y:y + h, z:z + d] = True
        ax.voxels(voxel, facecolors=color, edgecolor='none')

    # Set the aspect of the plot to be equal
    ax.set_xlim([0, amax])
    ax.set_ylim([0, amax])
    ax.set_zlim([0, amax])
    ax.set_box_aspect([1,1,1])


    plt.show()

if __name__ == "__main__":
    main()
