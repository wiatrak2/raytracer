all: raytracer

raytracer:
	ocamlc -o raytracer vec.ml material.ml ray.ml intersection.ml sphere.ml camera.ml world.ml output.ml trace.ml raytracer.ml

clean:
	rm *.c* raytracer

.PHONY: clean raytracer