# Copyright 2016 Daniel Nüst
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' An S4 class to represent a Dockerfile's maintainer
#'
#' @slot name The name
#' @slot email The email
#'
#' @return an object of class \code{Maintainer}
#' @export
Maintainer <- setClass("Maintainer",
                       slots = list(name = "character",
                                    email = "character"))

#' An S4 class to represent a Dockerfile
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/}.
#'
#' @slot image the base image, used in the FROM statement
#' @slot maintainer the MAINTAINER
#' @slot instructions an ordered list of instructions in the Dockerfile
#'
#' @return an object of class \code{Dockerfile}
#' @export
Dockerfile <- setClass("Dockerfile",
                    slots = list(image = "character",
                                 maintainer = "Maintainer",
                                 instructions = "list")
)



