# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

order_vertices <- function(vertices){
  
  vertices <- copy(vertices)
  countries <- colnames(vertices)
  colnames(vertices) <- c("country1", "country2")
  vertices_complex <- vertices[, complex := complex(real = country1 - mean(country1), imaginary = country2 - mean(country2))]
  vertices_complex <- vertices_complex[, angle := ((Arg(complex)/ pi * 180))]
  vertices_complex <- vertices_complex[order(angle)]
  
  vertices_final <- vertices_complex[, .(country1, country2)]
  colnames(vertices_final) <- countries
  
  vertices_final
  
}
