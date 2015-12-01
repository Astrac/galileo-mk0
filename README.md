# Galileo

[![Build status](https://travis-ci.org/Astrac/galileo.svg?branch=master)](https://travis-ci.org/Astrac/galileo)

## Intro

This project contains some expeirments in creating a physics engine for scala/scala-js. This is what is provided so far:

* An integrator based on the Runge-Kutta 4 algorithm that can support any set of types that implement typeclasses for State, Time and Derivative
* Boilerplate-free generation of State, Time and Derivative typeclasses based on shapeless (works only with `case class`es, `sealed trait`s are not supported)
