import time
import math
import sys
import numpy as np
from PIL import Image
from joblib import Parallel, delayed
import multiprocessing

def normalize(v):
    return v / np.linalg.norm(v) 

scene_width = 1024
scene_halfwidth = scene_width / 2
scene_pixels = scene_width ** 2

camera_pos = np.array([0, 6, -3])
camera_lookat = np.array([0, 5, 8])



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
        self.inten = intensity

    def distance(self, pos):
        return math.inf

    def direction(self, pos):
        return -self.dir

    def intensity(self, pos):
        return self.inten

class PointLight:
    def __init__(self, pos, color, intensity):
        self.pos = pos
        self.color = color
        self.inten = intensity

    def distance(self, pos):
        between = self.pos - pos
        return np.linalg.norm(between)

    def direction(self, pos):
        return normalize(self.pos - pos)

    def intensity(self, pos):
        radius = np.linalg.norm(self.pos - pos)
        return self.inten / (4 * math.pi * radius ** 2)

class Material:
    def __init__(self, color, albedo, reflectivity, index):
        self.color = color
        self.albedo = albedo
        self.reflectivity = reflectivity
        self.index = index


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

    def intersect(self, ray):
        between = self.pos - ray.pos
        adj = np.dot(between, ray.dir)
        dist = math.sqrt(np.dot(between, between) - (adj ** 2))

        if dist > self.radius:
            return None

        chord = math.sqrt(self.radius ** 2 - dist ** 2)
        t0 = adj - chord
        t1 = adj + chord

        if t0 < 0 and t1 < 0:
            return None

        t = t0 if t0 < t1 else t1        
        return ray.pos + (ray.dir * t)

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

    def intersect(self, ray):
        return ray.pos + (ray.dir * self.distance(ray))

scene_lights = [
    Light(vec(-1, -1, 0.4), vec(1, 1, 1), 20),
    PointLight(vec(0, 0, 3), vec(0.8, 0.6, 0.3), 10000),
    PointLight(vec(-20, 2, 18), vec(0.8, 0.45, 0.3), 10000),
    PointLight(vec(18, 2, 18), vec(0, 0.7, 0.7), 10000),
    PointLight(vec(0, 5, 5), vec(0.8, 0.6, 0.3), 100),
    PointLight(vec(-3, 5, 8), vec(0.8, 0.6, 0.3), 100),
    PointLight(vec(3, 5, 8), vec(0.8, 0.6, 0.3), 100),
    PointLight(vec(0, 8, 8), vec(0.8, 0.6, 0.3), 100),

]
        
scene_spheres = [
    Sphere(np.array([0, 5, 8]), 2, Material(np.array([0.99, 0.71, 0.08]), 0.18, 0.2, 0)),  # Center
    Sphere(np.array([7.36, 8.126, 8]), .5, Material(np.array([0, 0.2, 0.38]), 0.18, 0.6, 0)),  # 1
    Sphere(np.array([-7.36, 1.874, 8]), .5, Material(np.array([0, 0.2, 0.38]), 0.18, 0.6, 0)),  # 5
    Sphere(np.array([5.21, 7.21, 2.32]), .5, Material(np.array([0, 0.2, 0.38]), 0.18, 0.6, 0)),  # 8
    Sphere(np.array([-5.21, 2.8, 2.32]), .5, Material(np.array([0, 0.2, 0.38]), 0.18, 0.6, 0)),  # 6
    Sphere(np.array([5.21, 7.21, 13.68]), .5, Material(np.array([0, 0.2, 0.38]), 0.18, 0.6, 0)),  # 2
    Sphere(np.array([-5.21, 2.8, 13.68]), .5, Material(np.array([0, 0.2, 0.38]), 0.18, 0.6, 0)),  # 4
    Sphere(np.array([0, 5, 0]), .5, Material(np.array([0, 0.2, 0.38]), 0.18, 0.6, 0)),  # 7
    Sphere(np.array([0, 5, 16]), .5, Material(np.array([0, 0.2, 0.38]), 0.18, 0.6, 0)),  # 3
    Sphere(np.array([-18, 18, 17]), 1, Material(np.array([1, 1, 1]), 0.18, 0.75, 0)),
    Sphere(np.array([3, 3, 0]), 1, Material(np.array([1, 1, 1]), 0.18, 0.75, 0)),
]

scene_planes = [
    Plane(vec(0, 0, 100), vec(0, 0, -1), Material(vec(0.13, 0.13, 0.13), 0.18, .32, 0)),
    Plane(vec(0, -2, 0), vec(0, 1, 0), Material(vec(0.13, 0.13, 0.13), 0.18, .45, 0))
]

def nearest_object(ray, exclude = None):
    scene_objects = scene_spheres + scene_planes
    if exclude:
        scene_objects = [obj for obj in scene_objects if obj is not exclude]
    object_distances = [(obj, obj.distance(ray)) for obj in scene_objects]
    return min(object_distances, key=lambda pair: pair[1])

def transmission_ray(normal, incident, point, index):
    ref = normal
    n_t = index 
    n_i = 1

    dot = np.dot(incident, normal)
    if dot < 0.0:
        dot = -dot
    else:
        ref = -ref
        n_t, n_i = n_i, n_t
            
    ratio = n_i / n_t
    k = 1 - (ratio ** 2) * (1 - np.dot(dot, dot) ** 2)
    if k < 0:
        print('Ah fuk')
        return None
    else:
        return Ray(point, (point + dot * ref) * ratio - ref * math.sqrt(k))

def fresnel(normal, incident, index):
    dot = np.dot(incident, normal)
    n_t = index
    n_i = 1

    if dot > 0:
        n_t, n_i = n_i, n_t

    sin_t = math.sqrt(max(n_i / n_t * (1 - dot ** 2), 0))
    if sin_t > 1.0:
        return 1
    else:
        cos_t = math.sqrt(max(1 - sin_t ** 2, 0))
        cos_i = abs(cos_t)
        r_s = ((n_t * cos_i) - (n_i * cos_t)) / ((n_t * cos_i) + (n_i * cos_t))
        r_p = ((n_i * cos_i) - (n_t * cos_t)) / ((n_i * cos_i) + (n_t * cos_t))
        return (r_s * r_s + r_p * r_p) / 2

max_recursion_depth = 200

def trace(ray, steps = 0, total_distance = 0, tf = False, exclude = None):
    obj, distance = nearest_object(ray, exclude)
    if steps == max_recursion_depth or ray.pos[2] > 100 or distance > 100:
        if tf:
            return (False, None)
        return np.array([0, 0, 0])
    elif distance < 0.0001:
        if tf:
            return (True, total_distance)
        
        surface_normal = obj.normal(ray)

        ambient = 0.1
        color = obj.material.color * ambient

        # Lighting
        for light in scene_lights:
            light_direction = light.direction(ray.pos)

            shadow_ray = Ray(ray.pos, light_direction)
            shadow_trace = trace(shadow_ray, 0, 0, True, obj)
            
            in_light = not shadow_trace[0] or shadow_trace[1] > light.distance(ray.pos)
            light_intensity = light.intensity(ray.pos) if in_light else 0
            light_power = max(np.dot(surface_normal, light_direction), 0) * light_intensity
            light_reflected = obj.material.albedo / math.pi

            light_color = light.color * light_power * light_reflected

            color = color + obj.material.color * light_color

            
        if obj.material.reflectivity is not 0:
            reflectivity = obj.material.reflectivity
            reflection_ray = Ray(ray.pos, ray.dir - (2 * np.dot(ray.dir, surface_normal) * surface_normal))
            color = color * (1 - reflectivity)
            color = color + trace(reflection_ray, steps + 1, 0, False, obj) * reflectivity;
        elif obj.material.index is not 0:
            surface_color = obj.material.color
            refract_color = vec(0, 0, 0)

            ray = transmission_ray(surface_normal, ray.dir, ray.pos, obj.material.index)
            color = trace(ray, steps + 1, 0, False, obj) 
            
            #kr = fresnel(ray.dir, surface_normal, obj.material.index)
            #if kr < 1:
            #    ray = transmission_ray(surface_normal, ray.dir, ray.pos, obj.material.index)
            #    refract_color = trace(ray, steps + 1, 0, False, obj)

            #reflection_ray = Ray(ray.pos, ray.dir - (2 * np.dot(ray.dir, surface_normal) * surface_normal))
            #reflection_color = trace(reflection_ray, steps + 1, 0, False, obj)

            #color = reflection_color * kr + refract_color * (1 - kr)
            #transparency = 1.0
            #color = color * transparency * surface_color
            
        return np.clip(color, 0, 1)
    else:
        ray.iterate(distance)
        return trace(ray, steps + 1, total_distance + distance, tf, exclude)

def sample(x, y):
    return trace(Ray(camera_pos, ray_direction(x, y))) * 255

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
