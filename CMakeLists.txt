cmake_minimum_required(VERSION 3.5)

project(c-X11-hello-world-intro2 LANGUAGES C)

add_executable(c-X11-hello-world-intro2 main.c)

target_link_libraries(c-X11-hello-world-intro2 m)
target_link_libraries(c-X11-hello-world-intro2 X11)
