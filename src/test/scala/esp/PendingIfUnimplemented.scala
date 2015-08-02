package esp

import org.scalatest._

trait PendingIfUnimplemented extends SuiteMixin { this: Suite =>

  abstract override def withFixture(test: NoArgTest): Outcome = {
    super.withFixture(test) match {
      case Failed(ex: NotImplementedError) => Pending
      case other => other
    }
  }
}
