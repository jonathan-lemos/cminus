/*
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

private object B {
	def u2(s: Short)      : Seq[Byte] = ByteBuffer.allocate(2).putShort(s).array
	def u4(i: Int)        : Seq[Byte] = ByteBuffer.allocate(4).putInt(i).array
	def float(f: Float)   : Seq[Byte] = ByteBuffer.allocate(4).putFloat(f).array
	def long(l: Long)     : Seq[Byte] = ByteBuffer.allocate(8).putLong(l).array
	def double(d: Double) : Seq[Byte] = ByteBuffer.allocate(8).putDouble(d).array
}

sealed trait ByteStreamable {
	def toBytes: Seq[Byte]
}

sealed trait CpInfoTag extends ByteStreamable
final case class CpInfoClass(nameIndex: Short) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(7)) ++ B.u2(nameIndex)
}
final case class CpInfoFieldref(classIndex: Short, nameAndTypeIndex: Short) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(9)) ++ B.u2(classIndex) ++ B.u2(nameAndTypeIndex)
}
final case class CpInfoMethodref(classIndex: Short, nameAndTypeIndex: Short) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(10)) ++ B.u2(classIndex) ++ B.u2(nameAndTypeIndex)
}
final case class CpInfoInterfaceMethodref(classIndex: Short, nameAndTypeIndex: Short) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(11)) ++ B.u2(classIndex) ++ B.u2(nameAndTypeIndex)
}
final case class CpInfoString(stringIndex: Short) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(8)) ++ B.u2(stringIndex)
}
final case class CpInfoInt(i: Int) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(3)) ++ B.u4(i)
}
final case class CpInfoFloat(f: Float) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(4)) ++ B.float(f)
}
final case class CpInfoLong(l: Long) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(5)) ++ B.long(l)
}
final case class CpInfoDouble(d: Double) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(6)) ++ B.double(d)
}
final case class CpInfoNameAndType(nameIndex: Short, descriptorIndex: Short) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(12)) ++ B.u2(nameIndex) ++ B.u2(descriptorIndex)
}
final case class CpInfoUtf8(bytes: Seq[Byte]) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(1)) ++ B.u2(bytes.length.toShort) ++ bytes
}
final case class CpInfoMethodHandle(refKind: Byte, refIndex: Short) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(15)) ++ Seq(refKind) ++ B.u2(refIndex)
}
final case class CpInfoMethodType(descriptorIndex: Short) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(16)) ++ B.u2(descriptorIndex)
}
final case class CpInfoInvokeDynamic(bootstrapMethodAttrIndex: Short, nameAndTypeIndex: Short) extends CpInfoTag {
	override def toBytes: Seq[Byte] = Seq(Byte(18)) ++ B.u2(bootstrapMethodAttrIndex) ++ B.u2(nameAndTypeIndex)
}

final case class AttributeInfo(index: Short, info: Seq[Byte]) extends ByteStreamable {
	override def toBytes: Seq[Byte] = B.u2(index) ++ B.u4(info.length) ++ info
}
final case class FieldInfo(accessFlags: Short, nameIndex: Short, descriptorIndex: Short, attributes: Seq[AttributeInfoTag]) extends ByteStreamable {
	override def toBytes: Seq[Byte] = B.u2(accessFlags) ++ B.u2(nameIndex) ++ B.u2(descriptorIndex) ++ B.u2(attributes.length.toShort) ++ attributes.flatMap(_.toBytes)
}
final case class MethodInfo(accessFlags: Short, nameIndex: Short, descriptorIndex: Short, attributes: Seq[AttributeInfoTag]) extends ByteStreamable {
	override def toBytes: Seq[Byte] = B.u2(accessFlags) ++ B.u2(nameIndex) ++ B.u2(descriptorIndex) ++ B.u2(attributes.length.toShort) ++ attributes.flatMap(_.toBytes)
}

sealed trait AttributeInfoTag extends ByteStreamable {val nameIndex: Short}
final case class AttributeConstantValue(nameIndex: Short, constIndex: Short) extends AttributeInfoTag {
	override def toBytes: Seq[Byte] = B.u2(nameIndex) ++ B.u4(2) ++ B.u2(constIndex)
}
final case class exceptionEntry(startPc: Short, endPc: Short, handlerPc: Short, catchType: Short) extends ByteStreamable {
	override def toBytes: Seq[Byte] = B.u2(startPc) ++ B.u2(endPc) ++ B.u2(handlerPc) ++ B.u2(catchType)
}
final case class AttributeCode(nameIndex: Short, maxOperandStack: Short, maxNumLocals: Short, code: Seq[Byte], exceptionTable: Seq[exceptionEntry], attributes: Seq[AttributeInfoTag]) extends AttributeInfoTag {
	override def toBytes: Seq[Byte] = B.u2(nameIndex) ++ B.u4(101010) ++ B.u2(maxOperandStack) ++ B.u2(maxNumLocals) ++ B.u4(code.length) ++ code ++ B.u2(exceptionTable.length.toShort) ++ exceptionTable.flatMap(_.toBytes) ++ B.u2(attributes.length.toShort) ++ attributes.flatMap(_.toBytes)
}
sealed abstract class VerificationTypeInfo extends ByteStreamable
sealed trait StackMapFrame extends ByteStreamable
final case class SameFrame() extends StackMapFrame {
	override def toBytes: Seq[Byte] = Seq(Byte(0))
}
final case class SameLocals1StackItemFrame() extends StackMapFrame {
	override def toBytes: Seq[Byte] = Seq(Byte(64))
}
final case class SameLocals1StackItermFrameExtended() extends StackMapFrame {
	override def toBytes: Seq[Byte] = Seq(Byte(247))
}
final case class ChopFrame() extends StackMapFrame {
	override def toBytes: Seq[Byte] = Seq(Byte(248))
}
final case class stackMapFrame(st)
final case class AttributeStackMapTable(nameIndex: Short, )


final case class ClassData() {
	val magic              : Int         = 0xCAFEBABE
	val minor_version      : Short       = 8
	val major_version      : Short       = 1
	val constant_pool_count: Short       = 0
	val constant_pool      : Seq[CpInfoTag] = Seq()
	val access_flags       : Short       = 0
	val this_class         : Short       = 0
	val super_class        : Short       = 0
	val interfaces_count   : Short       = 0
	val interfaces         : Seq[Short]  = Seq()
	val fields_count       : Short       = 0
	val fields             : Seq[Short]  = Seq()
	val methods_count      : Short       = 0
}

object CodeGenerator {

}
*/