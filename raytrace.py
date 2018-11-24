import time
import sys
import numpy as np
from PIL import Image
from joblib import Parallel, delayed
import multiprocessing

def normalize(v):
    return v / np.linalg.norm(v) 

scene_width = 256
scene_halfwidth = scene_width / 2
scene_pixels = scene_width ** 2

camera_pos = np.array([0, 0, 0])
camera_lookat = np.array([0, 2, 4])
camera_focal = 1

camera_forward = normalize(camera_lookat - camera_pos)
camera_right = normalize(np.cross(np.array([0, 1, 0]), camera_forward))
camera_up = normalize(np.cross(camera_forward, camera_right))

def ray_direction(x, y):
    vec_x = (x / scene_halfwidth) * camera_right
    vec_y = (y / scene_halfwidth) * camera_up
    vec_z = camera_focal * camera_forward
    return normalize(vec_x + vec_y + vec_z)

def nearest_object(pos):
    center = np.array([0, 2, 4])
    radius = 2
    between = pos - center
    return np.linalg.norm(between) - radius

def trace(pos, dir, steps = 0, distance = 0):
    nearest = nearest_object(pos)
    if steps == 200 or pos[2] > 40 or nearest > 100:
        return False
    elif nearest < 0.001:
        return True
    else:
        return trace(pos + (dir * nearest), dir, steps + 1, distance + nearest)

def sample(x, y):
    return np.array([0, 255, 0]) if trace(camera_pos, ray_direction(x, y)) else np.array([0, 0, 0])

def supersample(x, y):
    return (sample(x + 0.25, y + 0.25) + sample(x - 0.25, y + 0.25) + sample(x + 0.25, y - 0.25) + sample(x - 0.25, y - 0.25)) / 4

start = time.time()

def render_row(x):
    sys.stdout.write(f'Progress: {format((x / scene_width) * 100, ".2f")}%')
    sys.stdout.write('\r')
    return np.array([supersample(x - scene_halfwidth, y - scene_halfwidth) for y in range(0, scene_width)], dtype='uint8')

num_cores = multiprocessing.cpu_count()

rows = Parallel(n_jobs=num_cores)(delayed(render_row)(x) for x in range(0, scene_width))
data = np.rot90(np.array(rows))

finish = time.time()
print(f'\nFinished in {finish-start}s\n')
        
img = Image.fromarray(data)
img.show()
