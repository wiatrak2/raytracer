all: raytracer

raytracer:
	ocamlfind ocamlopt -g -o raytracer vec.ml material.ml ray.ml intersection.ml sphere.ml plane.ml object.ml lightSource.ml camera.ml world.ml output.ml trace.ml light.ml raytracer.ml input.ml main.ml -package yojson -linkpkg
	rm -f *.c* *.o 
clean:
	rm -f *.c* *.o raytracer output.ppm

.PHONY: clean raytracer