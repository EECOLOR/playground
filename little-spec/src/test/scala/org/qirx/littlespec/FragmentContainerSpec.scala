package org.qirx.littlespec

object FragmentContainerSpec extends Specification {

  def todoResult(name: String) = Pending(name, "TODO")

  "FragmentContainer should" - {

    "provide a way to create a fragment" - {
      val fragmentContainer = new FragmentContainer {
        createFragment("test", {})
      }
      fragmentContainer.executeFragments() is
        Seq(todoResult("test"))
    }

    "provide a convenient way to create a fragment" - {

      new FragmentContainer {
        "test" - {}
      }

      success
    }

    "be able to execute fragments resulting in" - {

      "no results if empty" - {
        val container = new FragmentContainer {}
        container.executeFragments() is
          Seq.empty
      }

      "result of one fragment if one is present" - {
        val container =
          new FragmentContainer {
            "test" - {}
          }
        container.executeFragments() is
          Seq(todoResult("test"))
      }

      "correct results of nested fragments" - {
        val container =
          new FragmentContainer {
            "test1" - {
              "test2" - {}
              "test3" - {}
            }
          }

        container.executeFragments() is
          Seq(CompoundResult("test1", Seq(todoResult("test2"), todoResult("test3"))))

        // and being able to repeat it
        container.executeFragments() is
          Seq(CompoundResult("test1", Seq(todoResult("test2"), todoResult("test3"))))
      }
    }
  }
}