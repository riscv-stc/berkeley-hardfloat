package hardfloat

import chisel3._
import consts._
import chisel3.util._

class MulRecFNToFullRaw(val expWidth: Int = 3, val sigWidth: Int = 3) extends RawModule
{
  val io = IO(new Bundle {
    val a            = Input(UInt((expWidth+sigWidth+1).W))
    val b            = Input(UInt((expWidth+sigWidth+1).W))
    val invalidExc   = Output(Bool())
    val out_isNaN    = Output(Bool())
    val out_isInf    = Output(Bool())
    val out_isZero   = Output(Bool())
    val out_sign     = Output(Bool())
    val out_sExp     = Output(SInt((expWidth+2).W))
    val out_sig      = Output(UInt((sigWidth*2).W))
  })

  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val rawA = rawFloatFromRecFN(expWidth, sigWidth, io.a)
  val rawB = rawFloatFromRecFN(expWidth, sigWidth, io.b)
  val isSigNaNA = isSigNaNRawFloat(rawA)
  val isSigNaNB = isSigNaNRawFloat(rawB)
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val notSigNaN_invalidExc = (rawA.isInf && rawB.isZero) || (rawA.isZero && rawB.isInf)
  val notNaN_isInfOut      = rawA.isInf  || rawB.isInf
  val notNaN_isZeroOut     = rawA.isZero || rawB.isZero
  val notNaN_signOut       = rawA.sign   ^  rawB.sign
  val common_sExpOut       = rawA.sExp + rawB.sExp - (1<<expWidth).asSInt
  val common_sigOut        = rawA.sig  * rawB.sig
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  io.invalidExc := isSigNaNA || isSigNaNB || notSigNaN_invalidExc
  io.out_isInf  := notNaN_isInfOut
  io.out_isZero := notNaN_isZeroOut
  io.out_sExp   := common_sExpOut
  io.out_isNaN  := rawA.isNaN || rawB.isNaN
  io.out_sign   := notNaN_signOut
  io.out_sig    := common_sigOut
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
class MulRecFNToRaw(val expWidth: Int = 3, val sigWidth: Int = 3) extends RawModule
{
  val io = IO(new Bundle {
    val a            = Input(UInt((expWidth+sigWidth+1).W))
    val b            = Input(UInt((expWidth+sigWidth+1).W))
    val invalidExc   = Output(Bool())
    val out_isNaN    = Output(Bool())
    val out_isInf    = Output(Bool())
    val out_isZero   = Output(Bool())
    val out_sign     = Output(Bool())
    val out_sExp     = Output(SInt((expWidth+2).W))
    val out_sig      = Output(UInt((sigWidth+3).W))
  })

  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val rawA = rawFloatFromRecFN(expWidth, sigWidth, io.a)
  val rawB = rawFloatFromRecFN(expWidth, sigWidth, io.b)
  val isSigNaNA = isSigNaNRawFloat(rawA)
  val isSigNaNB = isSigNaNRawFloat(rawB)
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val notSigNaN_invalidExc = (rawA.isInf && rawB.isZero) || (rawA.isZero && rawB.isInf)
  val notNaN_isInfOut      = rawA.isInf  || rawB.isInf
  val notNaN_isZeroOut     = rawA.isZero || rawB.isZero
  val notNaN_signOut       = rawA.sign   ^  rawB.sign
  val common_sExpOut       = rawA.sExp + rawB.sExp - (1<<expWidth).asSInt
  val sigProd              = rawA.sig  * rawB.sig
  val common_sigOut        = Cat(sigProd(sigWidth*2-1, sigWidth-2), 
                                sigProd(sigWidth-3, 0).orR)
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  io.invalidExc := isSigNaNA || isSigNaNB || notSigNaN_invalidExc
  io.out_isInf  := notNaN_isInfOut
  io.out_isZero := notNaN_isZeroOut
  io.out_sExp   := common_sExpOut
  io.out_isNaN  := rawA.isNaN || rawB.isNaN
  io.out_sign   := notNaN_signOut
  io.out_sig    := common_sigOut
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
class MulRecFN(val expWidth: Int = 3, val sigWidth: Int = 3) extends RawModule
{
  val io = IO(new Bundle {
    val a              = Input(UInt((expWidth+sigWidth+1).W))
    val b              = Input(UInt((expWidth+sigWidth+1).W))
    val roundingMode   = Input(UInt(3.W))
    val out            = Output(UInt((expWidth+sigWidth+1).W))
    val exceptionFlags = Output(UInt(5.W))
  })

  val mult = Module(new MulRecFNToRaw(expWidth, sigWidth))
  mult.io.a            := io.a
  mult.io.b            := io.b

  val multRawOut = Wire(new RawFloat(expWidth, sigWidth+2))
  multRawOut.isNaN     := mult.io.out_isNaN
  multRawOut.isInf     := mult.io.out_isInf
  multRawOut.isZero    := mult.io.out_isZero
  multRawOut.sign      := mult.io.out_sign
  multRawOut.sExp      := mult.io.out_sExp
  multRawOut.sig       := mult.io.out_sig
  
  val roundRawFNToRecFN = Module(new RoundRawFNToRecFN(expWidth, sigWidth, 0))
  roundRawFNToRecFN.io.invalidExc     := mult.io.invalidExc
  roundRawFNToRecFN.io.infiniteExc    := false.B
  roundRawFNToRecFN.io.in             := multRawOut
  roundRawFNToRecFN.io.roundingMode   := io.roundingMode
  roundRawFNToRecFN.io.detectTininess := tininess_afterRounding

  io.out            := roundRawFNToRecFN.io.out
  io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}