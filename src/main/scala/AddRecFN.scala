
package hardfloat

import chisel3._
import chisel3.util._
import consts._

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
class AddRecFNToRaw(val expWidth: Int = 3, val sigWidth: Int = 3) extends RawModule
{
  val io = IO(new Bundle {
    val subOp        = Input(Bool())
    val a            = Input(UInt((expWidth+sigWidth+1).W))
    val b            = Input(UInt((expWidth+sigWidth+1).W))
    val roundingMode = Input(UInt(3.W))
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
  val alignDistWidth = log2Up(sigWidth)
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
    
  val rawA = rawFloatFromRecFN(expWidth, sigWidth, io.a)
  val rawB = rawFloatFromRecFN(expWidth, sigWidth, io.b)
  val isSigNaNA = isSigNaNRawFloat(rawA)
  val isSigNaNB = isSigNaNRawFloat(rawB)
  val effSignB  = Mux(io.subOp, !rawB.sign, rawB.sign)
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val eqSigns = (rawA.sign === effSignB)
  val notEqSigns_signZero = (io.roundingMode === round_min)
  val sDiffExps = rawA.sExp - rawB.sExp
  val modNatAlignDist = Mux(sDiffExps < 0.S, rawB.sExp-rawA.sExp, sDiffExps)
  val isMaxAlign = (sDiffExps >> alignDistWidth =/= 0.S) && 
                   ((sDiffExps >> alignDistWidth =/= -1.S) || (sDiffExps(alignDistWidth-1, 0) === 0.U))
  val alignDist = Mux(isMaxAlign, ((1 << alignDistWidth)-1).asUInt, modNatAlignDist.asUInt)
  val closeSubMags = !eqSigns && !isMaxAlign && (modNatAlignDist <= 1.S)
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val close_alignedSigA = Mux((0.S <= sDiffExps) &&  sDiffExps(0), (rawA.sig<<2).asSInt,              0.S((sigWidth+3).W)) | 
                          Mux((0.S <= sDiffExps) && !sDiffExps(0), Cat(0.U(1.W), rawA.sig<<1).asSInt, 0.S((sigWidth+3).W)) | 
                          Mux((sDiffExps < 0.S),                   Cat(0.U(2.W), rawA.sig).asSInt,    0.S((sigWidth+3).W))
  val close_sSigSum = close_alignedSigA - (rawB.sig<<1).asSInt
  val close_sigSum  = Mux(close_sSigSum < 0.S, -close_sSigSum, close_sSigSum)(sigWidth+1, 0).asUInt
  val close_adjustedSigSum    = close_sigSum << (sigWidth & 1)
  val close_reduced2SigSum    = orReduceBy2(close_adjustedSigSum)
  val close_normDistReduced2  = countLeadingZeros(close_reduced2SigSum)
  val close_nearNormDist      = (close_normDistReduced2 << 1)
  val close_sigOut            = ((close_sigSum<<close_nearNormDist)<<1)(sigWidth+2, 0)
  val close_totalCancellation = !(close_sigOut(sigWidth+2, sigWidth+1).orR)
  val close_notTotalCancellation_signOut = rawA.sign ^ (close_sSigSum < 0.S)
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
  val far_signOut    = Mux(sDiffExps < 0.S, effSignB, rawA.sign)
  val far_sigLarger  = Mux(sDiffExps < 0.S, rawB.sig, rawA.sig)
  val far_sigSmaller = Mux(sDiffExps < 0.S, rawA.sig, rawB.sig)
  val far_mainAlignedSigSmaller = Cat(far_sigSmaller, 0.U(5.W)) >> alignDist
  val far_reduced4SigSmaller    = orReduceBy4(Cat(far_sigSmaller, 0.U(2.W)))
  val far_roundExtraMask        = lowMask(alignDist>>2, (sigWidth + 5)>>2, 0)
  val far_alignedSigSmaller     = Cat(far_mainAlignedSigSmaller>>3,
                                     (far_mainAlignedSigSmaller(2,0).orR) || 
                                     ((far_reduced4SigSmaller & far_roundExtraMask).orR))
  val far_subMags = !eqSigns
  val far_negAlignedSigSmaller = Mux(far_subMags, Cat(1.U(1.W), ~far_alignedSigSmaller),
                                                  Cat(0.U(1.W), far_alignedSigSmaller))
  val far_sigSum = (far_sigLarger<<3) + far_negAlignedSigSmaller + far_subMags
  val far_sigOut = Mux(far_subMags, far_sigSum, far_sigSum>>1 | far_sigSum(0))

  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val notSigNaN_invalidExc = rawA.isInf && rawB.isInf && !eqSigns
  val notNaN_isInfOut      = rawA.isInf || rawB.isInf
  val addZeros             = rawA.isZero && rawB.isZero
  val notNaN_specialCase   = notNaN_isInfOut || addZeros
  val notNaN_isZeroOut     = addZeros || (!notNaN_isInfOut && closeSubMags && close_totalCancellation)
  val notNaN_signOut       = (eqSigns && rawA.sign) || (rawA.isInf && rawA.sign) || (rawB.isInf && effSignB) || 
                             (notNaN_isZeroOut && !eqSigns && notEqSigns_signZero) || 
                             (!notNaN_specialCase && closeSubMags && !close_totalCancellation && close_notTotalCancellation_signOut) || 
                             (!notNaN_specialCase && !closeSubMags && far_signOut)
  val common_sExpOut       = Mux(closeSubMags || (sDiffExps < 0.S), rawB.sExp, rawA.sExp) - 
                             Mux(closeSubMags, close_nearNormDist, far_subMags).asSInt
  val common_sigOut        = Mux(closeSubMags, close_sigOut, far_sigOut)

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
class AddRecFN(val expWidth: Int = 3, val sigWidth: Int = 3) extends RawModule
{
  val io = IO(new Bundle {
    val subOp          = Input(Bool())
    val a              = Input(UInt((expWidth+sigWidth+1).W))
    val b              = Input(UInt((expWidth+sigWidth+1).W))
    val roundingMode   = Input(UInt(3.W))
    val out            = Output(UInt((expWidth+sigWidth+1).W))
    val exceptionFlags = Output(UInt(5.W))
  })

  val adder = Module(new AddRecFNToRaw(expWidth, sigWidth))
  adder.io.subOp        := io.subOp
  adder.io.a            := io.a
  adder.io.b            := io.b
  adder.io.roundingMode := io.roundingMode

  val adderRawOut = WireInit(new RawFloat(expWidth, sigWidth+2))
  adderRawOut.isNaN     := adder.io.out_isNaN
  adderRawOut.isInf     := adder.io.out_isInf
  adderRawOut.isZero    := adder.io.out_isZero
  adderRawOut.sign      := adder.io.out_sign
  adderRawOut.sExp      := adder.io.out_sExp
  adderRawOut.sig       := adder.io.out_sig
  
  val roundRawFNToRecFN = Module(new RoundRawFNToRecFN(expWidth, sigWidth, 0))
  roundRawFNToRecFN.io.invalidExc     := adder.io.invalidExc
  roundRawFNToRecFN.io.infiniteExc    := false.B
  roundRawFNToRecFN.io.in             := adderRawOut
  roundRawFNToRecFN.io.roundingMode   := io.roundingMode
  roundRawFNToRecFN.io.detectTininess := tininess_afterRounding

  io.out            := roundRawFNToRecFN.io.out
  io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
