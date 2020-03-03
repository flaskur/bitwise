/* 
 * allEvenBits - return 1 if all even-numbered bits in word set to 1 where bits are numbered from 0 (least significant) to 31 (most significant)
 * Examples allEvenBits(0xFFFFFFFE) = 0, allEvenBits(0x55555555) = 1
 * Legal ops: ! ~ & ^ | + << >>
 */
int allEvenBits(int x)
{
  /* EXPLANATION OF LOGIC/ALGORITHM
   *
   * So we want a solution that has 01010101010101....010101 pattern.
   * The problem for this is that we can't simply use a value like 0x55555555 and just compare because restricted to 0xFF.
   * We do however know that the pattern MUST repeat. Which means that each HALF must be the same.
   * That is basically what we do. Continue to halve the value of x and check if x is equal to 1
   *
   * Logic behind this is that we need to compare both halves while simultaneously shortening it.
   */

  int sixteenShift = x >> 16;
  int eightShift;
  int fourShift;
  int twoShift;
  int result;

  x = x & (sixteenShift);
  eightShift = x >> 8;
  x = x & (eightShift);
  fourShift = x >> 4;
  x = x & (fourShift);
  twoShift = x >> 2;
  x = x & (twoShift);

  result = x & 1;
  return result;
}

/* 
 * bang - Compute !x without using !
 * Examples: bang(3) = 0, bang(0) = 1
 * Legal ops: ~ & ^ | + << >>
 */
int bang(int x)
{
  /* EXPLANATION OF LOGIC/ALGORITHM
   *
   * So a bang operation makes any zero value into 1 and any nonzero value into 0.
   * The main problem here is that you need to isolate 0 in such a way that no other number fulfills that condition.
   * Note that 0 has a special property such that the one's complement and two's complement are exact inverses of each other.
   * 
   * BIG POINT: For literally every other number the sign bit for the ones and two complement of that number will be the same.
   * So we can use this fact by extracting the MSB and check if they are equal.
   * Since we can't use ! to solve bang, we do a shift then AND with 1 to force it into either 1 or 0 output.
   *
   * The task was really just to isolate the condition of 0 but you had to know the complement property.
   */

  // Remember that onesComp(0) and twosComp(0) bitwise opposite and no other case is true.
  // This is because of the overflow property.
  int onesComplement = ~x;
  int twosComplement = onesComplement + 1;

  // So take the ~twosComplement = 111111111111 so if you AND them together you get all 1's. No other case fulfills this.
  // The fact is that the ones and twos complement of almost all integers is the same sign.
  // The only case where this is different is the 0 case
  int combinedInverseComplement = ~twosComplement & onesComplement;

  // So checkMSBSame will only return 1 if the MSB for the ones and twos complement are different.
  // Since onesComp(0) = 1111...11111 and twosComp(0) = 00000...0000, only they will be 1.
  int checkMSBSame = 1 & (combinedInverseComplement >> 31);

  // Returns 0 on any other case other than 0 input. Follows logic that !nonzero = zero.
  return checkMSBSame;
}

/* 
 * floatIsEqual - Compute f == g for floating point arguments f and g.
 * Both the arguments are passed as unsigned int's, but they are to be interpreted as the bit-level representations of single-precision floating point values. If either argument is NaN, return 0. +0 and -0 are considered equal.
 * Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 */
int floatIsEqual(unsigned uf, unsigned ug)
{
  /* EXPLANATION OF LOGIC/ALGORITHM
   * This one is actually pretty easy because we know that two FP numbers that have equal unsigned representation are also equal.
   * The only thing is to understand the weird cases like NaN, -0, +0 and handle them accordingly.
   *
   * For NaN you need to check if the exponents are all ones but the mantissa is not all 0's which would be +/-infinity depending on sign bit.
   * For zero case you just need to check if they are equal if you chop off the MSB.
   *
   */

  // We access the floating point section in single precision as the 1:9 bits.
  // So here we shift left by 1 to remove the sign bit and then arithmetic shift by 24 to make sure that the exponent is set to the right side.
  // Remember that with unsigned we do a logical shift and not an arithmetic shift which means logical right shifts do not copy msb.
  int exponentF = (uf << 1) >> 24;
  int exponentG = (ug << 1) >> 24;

  // To get the mantissa, we should just remove everything else by left shifting by 9 to remove the sign bit and exponent, then moving it back into position.
  // The important thing to note here is that since uf and ug and unsigned, the shifts are logical shifts and they don't maintain MSB on right shifts. Hence only 0's on each side with shifts.
  int mantissaF = (uf << 9) >> 9;
  int mantissaG = (ug << 9) >> 9;

  // Verify that NaN mean that the exponents are all 1 and the fraction is not 0.
  // Any instance of a NaN will return 0, not only both NaN.
  if ((exponentF == 0xFF && mantissaF != 0) || (exponentG == 0xFF && mantissaG != 0))
    return 0;
  else if ((uf << 1) == 0 && (ug << 1) == 0)
    return 1;
  else if (uf - ug == 0)
    return 1;

  // On all other cases just return 0 because they aren't equal.
  return 0;
}

/* 
 * floatUnsigned2Float - Return bit-level equivalent of expression (float) u
 * Result is returned as unsigned int, but it is to be interpreted as the bit-level representation of a single-precision floating point values. 
 * Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 */
unsigned floatUnsigned2Float(unsigned u)
{

  // Continue to iterate until we get the value of 0, which is how we know the E value in scientific form.
  unsigned copy = u;
  int counter = 0;
  int exponent;

  unsigned fractionInFront;
  unsigned fractionTruncate;
  unsigned floatingPoint;

  unsigned guardBit;
  unsigned roundBit;
  unsigned stickyBits;
  unsigned stickyBit;

  unsigned roundUp = 0;

  // ITS PROBABLY THE WHILE LOOP
  while (copy != 0)
  {
    copy = copy >> 1;
    counter++;
  }

  // Remember that counter should be the value of E - 1 because it notes the decimal place.
  // The while loop stops when it becomes 0, which is 1 place after the where the decimal should be.
  // Hence E in this case is counter - 1.

  // Formula is E = exponent - bias. But since single precision, bias is 2^(8-1) - 1 = 127.
  exponent = (counter - 1) + 127;

  // So now we want only the fractional part to be in the front. This is shift by 33 - counter b/c of inclusion of first 1.
  fractionInFront = u << (33 - counter);

  // Now we must handle the case where the fraction is greater than 23 bits. For now just test basic truncate.
  fractionTruncate = fractionInFront >> 9;

  // HERE IS WHERE WE CARE ABOUT ROUNDING.
  // WE NEED GUARD, ROUND, AND STICKY BIT
  // Guard is LSB after truncating. Round is the last number that was truncated. Sticky bits are all the following number that were truncated.

  // To get guard all you need to do as AND with 1 to get last number.
  guardBit = fractionTruncate & 1;

  // The round bit uses a similar logic but let shift be 1 less than truncate
  roundBit = (fractionInFront >> 8) & 1;

  // Now we need all the bits following truncate EXCEPT for round bit, since that is the last one truncated.
  // We can use a bit mask to get only the last 8 values.
  // Remember that 9 bits get truncated so the last 8 are the ones we want.
  stickyBits = 0xFF & fractionInFront;

  // printf("sticky bits are %u\n\n", stickyBits);

  // If I find a single 1, then the sticky bit turns to 0.
  stickyBit = !!stickyBits;

  // printf("guard %u, round %u, sticky %u\n\n", guardBit, roundBit, stickyBit);

  // Finally we can use these three bits to determine if we round up or not.
  if ((guardBit == 1 && roundBit == 1) || (roundBit == 1 && stickyBit == 1))
  {
    roundUp = 1;
  }

  // Converting to floating point is just Sign Bit, Exponent, and Fraction, but since u is unsigned signed bit is alway 0.
  floatingPoint = (exponent << 23) | fractionTruncate;

  // Make sure to round correctly...
  floatingPoint += roundUp;

  // Edge case, return 0 if the input is 0. Not normalized.
  if (u == 0)
    return 0;

  return floatingPoint;
}

/* 
 * isLess - if x < y  then return 1, else return 0 
 * Example: isLess(4,5) = 1.
 * Legal ops: ! ~ & ^ | + << >>
 */
int isLess(int x, int y)
{
  // First things first is I want to find out the MSB of the two values.
  int xMSB = x >> 31;
  int yMSB = y >> 31;

  // Alright in the case where they are different, that means the signs are different.
  // If x is -1 and y is 0, then immediately return 1.
  // If x is 0 and y is -1 then immediately return 0.
  // So notice that if the signs differ, we can immediately make a judgement about the answer.

  // We need a differentiator between same MSB or different MSB.
  // If xmsb and ymsb are different they return 1. If they are the same we expect 0.
  int diffSign = xMSB ^ yMSB;

  // Since we want to use diffSign for conditional, we want the form on -1 and 0 for easy AND conditions.
  // Note that to get -1 from 1, we need to take the two's complement. 0 is unaffected by this because of overflow.
  // So now if xmsb is different from ysmb, signDecider will be -1 and 0 for when they are the same.
  int signDecider = (~diffSign) + 1;

  // Say xmsb is 0 and ymsb is 1. We want to return false of 0. If I do xmsb | (!ymsb) I should get 0 | 0 which is 0.
  // In the same case but xmsb is 1 and ymsb is 0. I get 1 | 1 is 1.
  // So the equation xmsb | (!ymsb) follow and gets the correct value but we have to make sure that diffSign is 1.

  int differentCaseLogic = ((diffSign) & (xMSB | (!yMSB)));

  // Now we need to handle the same case logic. What happens when signs are equal?
  // If signs are equal then we do what we normally do and subtract from each other.
  // However, the reason why we even went through finding that the MSB is the same is because we want to remove it.
  // Why remove it? Because the 2's complement fails on the case where the value is tmin because it doesn't exist.
  // Also because arithmetic with really large numbers fails because of overflow.
  // So to handle that, we remove the MSB and force it to 0 so that overflow can still be accounted for.

  int xWithoutMSB = x & (~(1 << 31));
  int yWithoutMSB = y & (~(1 << 31));

  // Now we do the subtraction step which is the same as adding the twos complement.
  int difference = xWithoutMSB + (~yWithoutMSB + 1);

  // Now if the MSB of difference is 1, then we know overflow has occurred.
  // In normal case we just take the MSB and if it is 1 then it is negative and thus x < y.
  // If MSB is 0 then it is positive and thus x >= y.
  // I think the overflow case might mess everything up.

  // If negative then diffMSB goes to 1, if positive or 0 then it goes to 0.
  int diffMSB = !!(difference >> 31);

  int result = ((signDecider & differentCaseLogic) | ((~signDecider) & diffMSB));

  return result;
}

/* 
 * isNonNegative - return 1 if x >= 0, return 0 otherwise 
 * Example: isNonNegative(-1) = 0.  isNonNegative(0) = 1.
 * Legal ops: ! ~ & ^ | + << >>
 */
int isNonNegative(int x)
{
  // Follow the same logic as the sign bit function from our last homework.
  // We know that all negative numbers have signed bit of 1 and we also know that 0 and all positive numbers have a MSB of 0.
  // So to return 1 if x >= 0 means that we simply need to invert the value with a ! operator.
  // Remember that ~ behaves like ones complement and does not do the same thing as !.
  return !(x >> 31);
}

/*
 * isTmax - returns 1 if x is the maximum, two's complement number, and 0 otherwise 
 * Legal ops: ! ~ & ^ | +
 */
int isTmax(int x)
{
  /* EXPLANATION OF LOGIC/ALGORITHM
   *
   * So the professor actually gave us a hint about this problem. 
   * The most important part is that we have only two values that become 0 when multiplied by 2.
   * That is 1000000....000000 and 0 itself. But remember that Tmax is 011111111...11111.
   * So the goal here is to isolate zero and tmax, then distinguish between zero and tmax.
   *
   * Only zero * 2 and 1000000...0000 * 2 is equal to 0.
   * Separate case of -1 and tmax with MSB. We deal with -1 and tmax because zero = -1 + 1 and 1000000...000 = tmax + 1.
   *
   * If we have those two conditions, we can just AND them together such that only tmax will return 1.
   *
   */

  // To clarify, the hopefullyZero variable goes to zero if tmax and -1. So the job is just to differentiate between 01111111111111 and 111111111111 NOT 0.
  // No shifts to invert to 1000000...000000 and 0000000...00000. Then !! goes to 1 and 0.
  // So if x is tmax then we should get 1. If x is -1, the we should get 0.
  int mark = !!(~x);

  // Now we should have 111111111111111 and 0000000000000000 for those two cases. That is exactly what you wanted. So just AND with the result.

  // To isolate the cases such that only tmax and -1 are set to 11111111111...111111 we check if doubling the +1 value is equal to 0.
  int oneMSBIfTrue = x + 1;
  int hopefullyZero = oneMSBIfTrue + oneMSBIfTrue;

  // Notice here that mark will be 1 for all nonzero values. HopefullyZero will be 0 only for cases where x is -1 or x is tmax.
  // Thus combining them together will give you 1, only when the value is tmax.
  return (!hopefullyZero) & mark;
}

/* 
 * rotateLeft - Rotate x to the left by n
 * Can assume that 0 <= n <= 31
 * Examples: rotateLeft(0x87654321,4) = 0x76543218
 * Legal ops: ~ & ^ | + << >> !
 */
int rotateLeft(int x, int n)
{
  /* EXPLANATION OF LOGIC/ALGORITHM
   *
   * A rotate left means that you are basically shifting some n bits off and pushing it back into the right side.
   * The struggle with this is that you have to copy the parts that were shifted and actually insert it back without messing up everything
   *
   * The first part of the logic was to copy the bits that get shifted over.
   * To do that I made a bitmask by starting with 100000...00000 and arithmetic right shift by n - 1.
   * Why n - 1? Because We started with MSB of 1 so we can't include that.
   * Use and AND operation with the bit mask to capture those bits that get shifted.
   *
   * Next is to do the most obvious part of shifting x left by n. So x << n.
   *
   * The problem here is that we can't simply right shift the copy bits and OR them together because MSB will persist in arith shift.
   * Instead we make yet another bit mask but for the right side. If n = 4 then we want 0000000.....00001111.
   * We actually first aim for 111111111111.....1110000 by doing an inversion to take advantage of arith shift.
   * Starting with 1000000000....00000 we need to right shift by 32 - n - 1.
   * 
   * Now we can use the copy bits and right shift without worry because we simply AND with the bit mask for n bits from the right side.
   * We could not simply do a constant like 0x15, because the limit of constant is 0xFF which doesn't work for cases where n > 8.
   *
   * Note that this solution uses a lot of subtracting logic, but minus(-) operator isn't allowed. 
   * Subtracting is just adding the 2's complement as ~number + 1.
   * This only fails in cases where "number" is tmin because tmin's 2's complement is itself.
   *
   * So we just OR together the two bit portions and get a final answer.
   */

  // The first thing to do is copy the bits that are shifted out.
  int firstBitOne = 1 << 31;

  // Now the amount of bits we need to copy is based off the relationship n - 1 because we already have the first bit spot considered.
  // Note that you can't do n-1 because restriction, so doing 2's complement as 1's complement + 1, meaning ~ then +1.
  int copyMask = firstBitOne >> (n + ((~1) + 1));

  // Now we can use the copy mask to store the bits that would have been rotated out.
  // Note that we use a & operator which makes sense. The n number of bits in the front will maintain whatever they have and the rest of the bit will be forced into 0.
  int rotatedBits = x & copyMask;

  // Now that we have the stored rotatedBits, we can go ahead and shift the original value by n.
  // This was our first intuition. This will leave an n number of zeros on the right side.
  int shiftedBits = x << n;

  // The two's complement case fails to invert only on the most negative case (tmin) because there is no positive representation of the min value.
  int mask = ~(firstBitOne >> (32 + ((~n) + 1) + ((~1) + 1)));

  // Alright so now we have the mask for the lower part. So we just need to shift the copied part by 32 - n bits and use the bit mask to get only that part.
  int finalRotatedBits = (rotatedBits >> (32 + ((~n) + 1)) & mask);

  // Now we just combine the shifted with the rotated bits.
  return shiftedBits | finalRotatedBits;
}

/* 
 * signMag2TwosComp - Convert from sign-magnitude to two's complement where the MSB is the sign bit
 * Example: signMag2TwosComp(0x80000005) = -5.
 * Legal ops: ! ~ & ^ | + << >>
 */
int signMag2TwosComp(int x)
{
  /* EXPLANATION OF LOGIC/ALGORITHM
   *
   * We are actually given a signed magnitude and we are asked to convert into a 2's complement number.
   * This is actually not a problem for instances where x is 0 or positive. It is honestly the same value.
   * The problem arises when you deal with negative numbers because you have to isolate that case.
   * 
   * The first goal was to set the MSB of all numbers to 0 which we did by manipulating constants.
   * Then we know the result of x if we remove the MSB which is important because we are dealing with sign magnitude.
   * Now we assume two cases... If MSB is 1 and hence negative, then we need to return the 2's complement of x without the MSB as 1. 
   * Remember that we set all MSB to zero with zeroMSB operation.
   *
   * If MSB of x is 1 we know negative so return 2's of zeroMSB. If MSB of x is 0 then we know it is 0 or positive which means we don't need to do anything.
   * I divided into two "portions" and since MSBShift with be either 1111111...111111 or 0000000...00000, only one value will be nonzero.
   * So finally just OR them together and return the value. You OR because one of them will be 0000...0000 but we don't know which portion.
   */

  // So first we want to save the MSB value and cut it off the actual test value.

  // Operations to force MSB to be zero on all given x. Should be 011111....111111.
  int oneInFront = 1 << 31;
  int invertedOneInFront = ~oneInFront;

  // If you AND with 0, it will always be 0. If you AND with 1, it will maintain whatever value it aleady has.
  // Hence, zeroMSB sets the front to 0 which is only significant for the negative case.
  int zeroMSB = invertedOneInFront & x;

  // If positive, do nothing, if negative do the inversion.
  // ~(firstBit - positive & 0 case)&(zeroMSB) || (firstBit - negative case is)&(~zeroMSB + )
  // So explain that.....
  // Instead of having firstBit as 10000000.... we just leave it has 11111111 which is -1. So we should probably just use a regular >> 31 right shift.
  // If the value is 0 or positive then...
  // ~(0000000)&(zeroMSB) | (00000000)&(~zeroMSB + 1)
  // So if postive, the zeroMSB | 000000000 which equals zeroMSB which is perfect.
  // If the value is negative then...
  // ~(1111111)&(zeroMSB) | (11111111)&(~zeroMSB + 1)
  // So if negative, then the value will actually be inverted as a twos complement.

  // This is the two's complement that we want to return in the negative sign magnitude but not 0 or positive x.
  int invertingLogic = ~zeroMSB + 1;

  // MSBShift is the decider which differentiates between negative and non negative numbers.
  int MSBShift = x >> 31;

  // Notice here that only one value will be nonnegative since MSBShift will be 111111....111111 or 00000000...0000000.
  // Since we AND with all 1's on one portion and all 0's on another portion, one portion will be 0 and another will be the correct answer.
  int firstPortion = (~MSBShift) & zeroMSB;
  int secondPortion = (MSBShift)&invertingLogic;

  // Now we simply or together to get the real answer since we don't know which portion is 00000...00000.
  return firstPortion | secondPortion;
}

/* 
 * thirdBits - return word with every third bit (starting from the LSB) set to 1
 * Legal ops: ! ~ & ^ | + << >>
 */
int thirdBits(void)
{
  /* EXPLANATION OF LOGIC/ALGORITHM
   *
   * Since every third bit is a 1 with LSB being a 1, then we already know what the final result will be.
   * The only difficult part is just building using the constraints of the constants as 0xFF limiting.
   * We expect 001001001...001001 which is 1227133513 in decimal. Our limits is 8 bits so I used 2 template bits to combine together to build the final product.
   *
   * 146 is 10010010 and 9 is 1001 in binary. 
   * I shifted by 9 each time to account for the zero's inbetween that are not accounted for.
   * I append the 1001 to the end to match the exact value.
   *
   */

  // Value in binary is 10010010 which I will use for the pattern of third repeats.
  int template1 = 146;

  // Value in binary is 1001 which is strictly for appending to the end.
  int template2 = 9;

  // I used a combination of shifts and ORs to build the pattern.
  int value = (((((template1 << 9) | template1) << 9) | template1) << 5) | template2;
  return value;
}