all: raytracer

raytracer:
	ocamlc -g -o raytracer vec.ml material.ml ray.ml intersection.ml sphere.ml camera.ml world.ml output.ml trace.ml raytracer.ml
	rm -f *.c*

debug:
	ocamlc -g -o raytracer vec.ml material.ml ray.ml intersection.ml sphere.ml camera.ml world.ml output.ml trace.ml raytracer.ml

clean:
	rm -f *.c* raytracer output.ppm

.PHONY: clean raytracer