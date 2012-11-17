#!/usr/bin/ioke

Ioke = LanguageExperiment with(
  goal: :expressiveness,
  data: as(code),
  code: as(data),
  features: [
    :dynamic,
    :object_oriented,
    :prototype_based,
    :homoiconic,
    :macros
  ],
  runtimes:(JVM, CLR),
  inspirations: set(Io, Smalltalk, Ruby, Lisp)
)

hello = method("Every example needs a hello world!",
  name,
  "hello, #{name}!" println)

Ioke inspirations select(
  features include?(:object_oriented)
) each(x, hello(x name))
