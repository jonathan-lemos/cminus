import java.nio.ByteBuffer

object AccessFlag {
	val PUBLIC    : Short = 0x0001
	val FINAL     : Short = 0x0010
	val SUPER     : Short = 0x0020
	val INTERFACE : Short = 0x0200
	val ABSTRACT  : Short = 0x0400
	val SYNTHETIC : Short = 0x1000
	val ANNOTATION: Short = 0x2000
	val ENUM      : Short = 0x4000
}

object Bytes {
	def fromShort(i: Short)  : Seq[Byte] = ByteBuffer.allocate(2).putShort(i).array
	def fromInt(i: Int)      : Seq[Byte] = ByteBuffer.allocate(4).putInt(i).array
	def fromFloat(f: Float)  : Seq[Byte] = ByteBuffer.allocate(4).putFloat(f).array
	def fromDouble(f: Double): Seq[Byte] = ByteBuffer.allocate(8).putDouble(f).array
	def fromLong(i: Long)    : Seq[Byte] = ByteBuffer.allocate(8).putLong(i).array
}

sealed trait ByteStreamable {
	def toBytes: Seq[Byte]
}

final case class AttributeInfo(index: Short, length: Int, info: Seq[Byte]) extends ByteStreamable {
	override def toBytes: Seq[Byte] = Bytes.fromShort(index) ++ Bytes.fromInt(length) ++ info
}

sealed trait CpInfoTag
final case class CpInfoClass(nameIndex: Short) extends CpInfoTag with ByteStreamable {
	override def toBytes: Seq[Byte] = Bytes.fromShort(7) ++ Bytes.fromShort(nameIndex)
}
final case class CpInfoFieldref(classIndex: Short, nameAndTypeIndex: Short) extends CpInfoTag with ByteStreamable {
	override def toBytes: Seq[Byte] = Bytes.fromShort(9) ++ Bytes.fromShort(classIndex) ++ Bytes.fromShort(nameAndTypeIndex)
}
final case class CpInfoMethodref(classIndex: Short, nameAndTypeIndex: Short) extends CpInfoTag with ByteStreamable {
	override def toBytes: Seq[Byte] = Bytes.fromShort(10) ++ Bytes.fromShort(classIndex) ++ Bytes.fromShort(nameAndTypeIndex)
}
final case class CpInfoInterfaceMethodref(classIndex: Short, nameAndTypeIndex: Short) extends CpInfoTag with ByteStreamable {
	override def toBytes: Seq[Byte] = Bytes.fromShort(11) ++ Bytes.fromShort(classIndex) ++ Bytes.fromShort(nameAndTypeIndex)
}

object CpInfoTag {
	val Class              : Byte = 7
	val Fieldref           : Byte = 9
	val Methodref          : Byte = 10
	val InterfaceMethodref : Byte = 11
	val String             : Byte = 8
	val Integer            : Byte = 3
	val Float              : Byte = 4
	val Long               : Byte = 5
	val Double             : Byte = 6
	val NameAndType        : Byte = 12
	val Utf8               : Byte = 1
	val MethodHandle       : Byte = 15
	val MethodType         : Byte = 16
	val InvokeDynamic      : Byte = 18
}

final case class CpInfo(tag: Byte, info: Seq[Byte]) extends ByteStreamable {
	override def toBytes: Seq[Byte] = Seq(tag) ++ info
}

final case class FieldInfo(access_flags: Short, name_index: Short, descriptor_index: Short, attributes_count: Short, attributes: Seq[AttributeInfo]) extends Serializable

final case class ClassData() {
	val magic              : Int         = 0xCAFEBABE
	val minor_version      : Short       = 8
	val major_version      : Short       = 1
	val constant_pool_count: Short       = 0
	val constant_pool      : Seq[CpInfo] = Seq()
	val access_flags       : Short       = 0
	val this_class         : Short       = 0
	val super_class        : Short       = 0
	val interfaces_count   : Short       = 0
	val interfaces         : Seq[Short]  = Seq()
	val fields_count       : Short       = 0
	val fields             : Seq[Short]  = 0
	val methods_count      : Short       = 0
}

object CodeGenerator {

}
