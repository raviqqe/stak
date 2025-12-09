---
title: Running Stak Scheme as Docker containers
description: How to run Docker images of Stak Scheme and its utility commands
---

This page describes how to run pre-built Docker images of Stak Scheme and its
utility commands. By reading this page, you will learn:

- What Docker images are available for the Stak Scheme interpreter and its
  utilities.
- How to run such Docker images on your local machine.

## Docker images

We provide several Docker images for Stak Scheme and its utilities.
The images are hosted on [Docker Hub](https://hub.docker.com/) as listed below.

| Image name        | Link                                                                                                                                                         | Description                  |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------- |
| `stak`            | [![Docker image size](https://img.shields.io/docker/image-size/raviqqe/stak?style=flat-square)](https://hub.docker.com/r/raviqqe/stak)                       | Interpreter                  |
| `stak-compile`    | [![Docker image size](https://img.shields.io/docker/image-size/raviqqe/stak-compile?style=flat-square)](https://hub.docker.com/r/raviqqe/stak-compile)       | Bytecode compiler            |
| `stak-decode`     | [![Docker image size](https://img.shields.io/docker/image-size/raviqqe/stak-decode?style=flat-square)](https://hub.docker.com/r/raviqqe/stak-decode)         | Bytecode decoder             |
| `stak-interpret`  | [![Docker image size](https://img.shields.io/docker/image-size/raviqqe/stak-interpret?style=flat-square)](https://hub.docker.com/r/raviqqe/stak-interpret)   | Bytecode interpreter         |
| `stak-profile`    | [![Docker image size](https://img.shields.io/docker/image-size/raviqqe/stak-profile?style=flat-square)](https://hub.docker.com/r/raviqqe/stak-profile)       | Bytecode profiler            |
| `stak-repl`       | [![Docker image size](https://img.shields.io/docker/image-size/raviqqe/stak-repl?style=flat-square)](https://hub.docker.com/r/raviqqe/stak-repl)             | REPL                         |
| `mstak`           | [![Docker image size](https://img.shields.io/docker/image-size/raviqqe/mstak?style=flat-square)](https://hub.docker.com/r/raviqqe/mstak)                     | Minimal interpreter          |
| `mstak-interpret` | [![Docker image size](https://img.shields.io/docker/image-size/raviqqe/mstak-interpret?style=flat-square)](https://hub.docker.com/r/raviqqe/mstak-interpret) | Minimal bytecode interpreter |

## Running Docker containers

### Interpreter

To run the Docker image of the `stak` interpreter, use the following command:

```sh
docker run -it --rm -v ./hello.scm:/hello.scm raviqqe/stak hello.scm
```

Note that you need mount the Scheme file onto the Docker container so that the
`stak` command can access it inside the container.

### REPL

To run the Docker image of the `stak-repl` REPL command, use the following
command:

```sh
docker run -it --rm raviqqe/stak-repl
```
