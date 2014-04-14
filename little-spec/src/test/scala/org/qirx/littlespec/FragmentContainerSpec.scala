package org.qirx.littlespec

object FragmentContainerSpec extends Specification {

  "FragmentContainer should" - {
    "provide a convenient way to create a section" - {

      new FragmentContainer {
        "test" - {}
      }

      success
    }

    "have no sections if empty" - {
      val container = new FragmentContainer {}
      container.fragments is Seq.empty
    }

    "detect when sections are created" - {
      val container =
        new FragmentContainer {
          "test" - {}
        }
      container.fragments.size is 1
    }

    "not respond to section creation events of sub fragments" - {
      val container =
        new FragmentContainer {
          "test1" - {
            "test2" - {}
            "test3" - {}
          }
        }

      container.fragments.size is 1
      container.fragments.head.execute()
      container.fragments.size is 1
    }
  }

}