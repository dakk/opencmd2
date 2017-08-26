cmake_minimum_required(VERSION 2.8)

project(OpenCmd2)

# Find dependancies
find_package(OpenGL REQUIRED)
find_package(OpenAL REQUIRED)
find_package(SDL2 REQUIRED)

include_directories(
	${SDL2_INCLUDE_DIR}
	${OPENGL_INCLUDE_DIR}
	${OPENAL_INCLUDE_DIR}
)
