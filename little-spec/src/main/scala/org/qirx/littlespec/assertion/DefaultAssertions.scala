package org.qirx.littlespec.assertion

trait DefaultAssertions
	extends ThrowingAssertions
	with TypeAssertions
	with CollectionAssertions { self: StaticAssertions with DefaultAssertEnhancements => }