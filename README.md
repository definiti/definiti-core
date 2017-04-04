# Definiti

> Definitively your model Definition

## About this project

This project is about creating a simple language to describes rule of your domain model.
Then Definiti compiler will transform it into your favorite language and framework.

The objective is to write all the rules at one place and compile it into every languages of your project.

💡 There is no willing to replace existing languages (yet).
The objective is to describe domain models and compile it into more technical languages.
Then these last will be used for every other things (side-effect operations, thread management, etc.).

## Get started

⚠ This project is the subject of active research.
It is not ready for production use yet.

This part describes how to test the compiler.
For more advanced usage, please come back later.

Clone the repository:

```sh
$ git clone git@github.com:definiti/definiti.git
```

Go into the project and launch sbt:

```
$ cd definiti
$ sbt run
```

It will read the file `src/main/resources/samples/first.def`
and save the results into `target/samples` directory.

Feel free to change the `first.def` file to test it.

## Roadmap (not ordered)

* [x] Create a first alpha alpha MVP
* [ ] Improve unit testing
* [ ] Create documentation
* [ ] Define a first complete-enough version of the core
* [ ] Compile to Scala with the defined core
* [ ] Compile to Typescript with the defined core
* [ ] Create Play JSON readers and writers
* [ ] Create Spray JSON readers and writers
* [ ] Create model error report in typescript (for form validation)
* [ ] Create PUML diagrams
* [ ] Redesign compiler to have independent executions without reload
* [ ] Improve error handling in compiler
* [ ] Module system
* [ ] Generic types (at least an Array system…)
* [ ] So many other things…