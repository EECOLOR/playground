package org.qirx.littlespec

object FragmentContainerSpec extends Specification {

  "Specification should" - {

    def todoResult(title: String) = Pending(Text(title), "TODO")

    "provide a way to create a fragment" - {
      val c = new Specification {
        createFragment("test", {})
      }

      c.executeFragments() is Seq(todoResult("test"))
    }

    "provide a convenient way to create a fragment" - {

      val c = new Specification {
        "test" - {}
      }

      c.executeFragments() is Seq(todoResult("test"))
    }

    "provide a way to use code as an example" - {

      def todoResult(text: String) = Pending(Code(text), "TODO")

      def result(message: String) =
        CompoundResult(Text("test"), Seq(todoResult(message)))

      "empty" - {
        val c = new Specification {
          "test" - example {}
        }
        c.executeFragments() is Seq(result(""))
      }

      "single line" - {
        val `this will result in a todo` = ()
        val expected = Seq(result("`this will result in a todo`"))

        "on a single line" - {
          val c = new Specification {
            "test" - example { `this will result in a todo` }
          }
          c.executeFragments() is expected
        }

        "on multiple lines" - {
          val c = new Specification {
            "test" - example {
              `this will result in a todo`
            }
          }
          c.executeFragments() is expected
        }
      }

      "multiline" - {
        def `this will` = ()
        val `result in a todo` = ()
        val c = new Specification {
          "test" - example {
            `this will`
            `result in a todo`
          }
        }
        c.executeFragments() is
          Seq(result("""|`this will`
              |`result in a todo`""".stripMargin))
      }

      "comments" - {
        val `result in a todo` = ()

        "single line" - {
          val c = new Specification {
            "test" - example {
              // this will { } } {
              `result in a todo`
            }
          }

          c.executeFragments() is
            Seq(result("""|// this will { } } {
                |`result in a todo`""".stripMargin))
        }

        "multiline on single line" - {
          val c = new Specification {
            "test" - example { /* this will { } } { */ `result in a todo` }
          }
          c.executeFragments() is
            Seq(result("/* this will { } } { */ `result in a todo`"))
        }

        "multiline" - {

          val c = new Specification {
            "test" - example {
              /*
               * this will { } } {
               */
              `result in a todo`
            }
          }
          c.executeFragments() is
            Seq(result("""|/*
                          | * this will { } } {
                          | */
                          |`result in a todo`""".stripMargin))
        }
      }

      "nested braces" - {
        val `result in a todo` = ()
        val c = new Specification {
          "test" - example {
            {
              `result in a todo`
            }
          }
        }
        c.executeFragments() is
          Seq(result("""|{
              |  `result in a todo`
              |}""".stripMargin))
      }
    }

    "be able to execute fragments resulting in" - {

      "no results if empty" - {
        val container = new Specification {}
        container.executeFragments() is Seq.empty
      }

      "result of one fragment if one is present" - {
        val container =
          new Specification {
            "test" - {}
          }
        container.executeFragments() is
          Seq(todoResult("test"))
      }

      "correct results of nested fragments" - {
        val container =
          new Specification {
            "test1" - {
              "test2" - {}
              "test3" - {}
            }
          }

        container.executeFragments() is
          Seq(CompoundResult(Text("test1"), Seq(todoResult("test2"), todoResult("test3"))))

        // and being able to repeat it
        container.executeFragments() is
          Seq(CompoundResult(Text("test1"), Seq(todoResult("test2"), todoResult("test3"))))
      }
    }
  }
}
