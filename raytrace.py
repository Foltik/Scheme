import time
import math
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
camera_lookat = np.array([0, 0, 4])
camera_focal = 1

camera_forward = normalize(camera_lookat - camera_pos)
camera_right = normalize(np.cross(np.array([0, 1, 0]), camera_forward))
camera_up = normalize(np.cross(camera_forward, camera_right))

def veclist(lst):
    return np.array(lst)

def vec(x, y, z):
    return np.array([x, y, z])

def ray_direction(x, y):
    vec_x = (x / scene_halfwidth) * camera_right
    vec_y = (y / scene_halfwidth) * camera_up
    vec_z = camera_focal * camera_forward
    return normalize(vec_x + vec_y + vec_z)


class Light:
    def __init__(self, dir, color, intensity):
        self.dir = dir
        self.color = color
        self.intensity = intensity


class Material:
    def __init__(self, color, albedo):
        self.color = color
        self.albedo = albedo


class Ray:
    def __init__(self, pos, dir):
        self.pos = pos
        self.dir = dir

    def iterate(self, t):
        self.pos = self.pos + self.dir * t


class Sphere:
    def __init__(self, pos, radius, material):
        self.pos = pos
        self.radius = radius
        self.material = material

    def distance(self, ray):
        between = ray.pos - self.pos
        return np.linalg.norm(between) - self.radius

    def normal(self, ray):
        between = ray.pos - self.pos
        return normalize(between)


class Plane:
    def __init__(self, pos, norm, material):
        self.pos = pos
        self.norm = norm
        self.material = material

    def distance(self, ray):
        denom = np.dot(-self.norm, ray.dir)
        if denom > .000001:
            between = self.pos - ray.pos
            dist = np.dot(between, -self.norm) / denom
            return dist if dist >= 0 else math.inf
        else:
            return math.inf

    def normal(self, ray):
        return self.norm

scene_global_light = Light(vec(0, -1, 0), vec(1, 1, 1), 50)
        
scene_spheres = [
    Sphere(np.array([0, 0, 5]), 1, Material(np.array([0.15, 0.8, 0.15]), 0.18)),
    Sphere(np.array([-3, 1, 6]), 2, Material(np.array([1.0, 0.2, 0.2]), 0.18)),
    Sphere(np.array([2, 1, 4]), 1.5, Material(np.array([0.2, 0.2, 1.0]), 0.18))
]

scene_planes = [
    Plane(vec(0, -2, 0), vec(0, 1, 0), Material(vec(0.13, 0.13, 0.13), 0.18)),
    Plane(vec(0, 0, 20), vec(0, 0, -1), Material(vec(0.5, 0.5, 0.5), 0.18))
]

def nearest_object(ray):
    scene_objects = scene_spheres + scene_planes
    object_distances = [(obj, obj.distance(ray)) for obj in scene_objects]
    return min(object_distances, key=lambda pair: pair[1])

def trace(ray, steps = 0, total_distance = 0, tf = False):
    obj, distance = nearest_object(ray)
    if steps == 200 or ray.pos[2] > 40 or distance > 100:
        if tf:
            return False
        return np.array([0, 0, 0])
    elif distance < 0.001:
        if tf:
            return True
        # cool stuff here
        surface_normal = obj.normal(ray)
        light_direction = -scene_global_light.dir

        shadow_ray = Ray(ray.pos + (ray.dir * distance) + (surface_normal * 0.000001), light_direction)

        light_intensity = scene_global_light.intensity
        if trace(shadow_ray, 0, 0, True):
            light_intensity = 0
        else:
            if isinstance(obj, Sphere):
                print('nope')
            
        light_power = max(np.dot(surface_normal, light_direction), 0.0) * light_intensity
        light_reflected = obj.material.albedo / math.pi
        light_color = scene_global_light.color
        ambient = vec(0.2, 0.2, 0.2)
        color = obj.material.color * light_color * light_power * light_reflected
        return np.clip(color, 0, 1)
    else:
        ray.iterate(distance)
        return trace(ray, steps + 1, total_distance + distance, tf)

def sample(x, y):
    return trace(Ray(camera_pos, ray_direction(x, y))) * 255

def supersample(x, y):
    return (sample(x + 0.25, y + 0.25) + sample(x - 0.25, y + 0.25) + sample(x + 0.25, y - 0.25) + sample(x - 0.25, y - 0.25)) / 4

start = time.time()

def render_row(x):
    sys.stdout.write(f'Progress: {format((x / scene_width) * 100, ".2f")}%')
    sys.stdout.write('\r')
    return np.array([sample(x - scene_halfwidth, y - scene_halfwidth) for y in range(0, scene_width)], dtype='uint8')

num_cores = multiprocessing.cpu_count()

rows = Parallel(n_jobs=num_cores)(delayed(render_row)(x) for x in range(0, scene_width))
data = np.rot90(np.array(rows))

finish = time.time()
print(f'\nFinished in {finish-start}s\n')
        
img = Image.fromarray(data)
img.show()
