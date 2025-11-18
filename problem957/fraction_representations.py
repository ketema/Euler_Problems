#!/usr/bin/env python3
"""
If g(16) = N/D is a fraction, what are ALL possible ways to represent it
as a single integer that PE would accept?
"""
from fractions import Fraction
from math import gcd

# The exact g(16)
N = 26881150706322905951141050230768841994215798918300659268478023670452065464132797155351545925638045960416792719623668451522048336747223311121568931679652404633241908409450511285996668257814798532832758440487120113302602184196329774222903365439850855686593447062304279233308312544843757668687077284577781348011905808210191952239869608135200543402316622450461023375424466742452224991888254340838108505131126200075213237632474856188652758589769304908735015692521026122926799960898166784484271097099807580362351810328584476319403926657103173562713594721305145582535302040818852433514155100995538676771314340065340761216001443192396998231577323655755270625875959688660622684789041070157664789067193637229138254489793398510048576144309639766458226199647453298981906000581396841595404738532874136282102515169686326180712783443506181577956793986537285512988585974195354534630024650344017963576307636262453972056579515848478731080100944756737060850470219815388739461081814862904272875624208671636738391275290010551144123293650842584219769755785413174308236284776281674731673277063475486699919888575745350504396050170954817280585906543630847958949079090878084132283750793726093944140860488587127650094001942439301254727977057788718732602969

D = 107163596189911017950956973960406298602557508261312162368794353906791469889499696253379610292058431389330318701815967280041913783959028940090904042850187322364210383997284931786021448773538536484069025810996180390907605753798773218473400022980246783057616470509852724413970993802567692508837481093727245924395473194254121472695081138812468301294149128147450428624058126882078512895816596394177955000056833763720767430390602258784707373521016433143603254757172172602264931911811087447257803195190699504187360500798080823253925107318748327307020479254126718556684200751892511302354998542813647779236292897396842815488479356427086661582211421446466010135298151730986983193561289898918665344197813390571025766643112966855464854894202066617623010188075779600620974433442957113687750829277861526591569274852656880236594117811851271600288528383554556632441065984638706028311863489449923583477815798298279611070455378976609531936544606248826578204032447803023231457673886100142627967395969812339707188641240885936172706336349944180619969581689787772779229189674299148204463157147649179648

print("="*70)
print("FRACTION REPRESENTATION ANALYSIS")
print("="*70)
print()

# Verify it's in simplest form
g = gcd(N, D)
print(f"GCD(N, D) = {g}")
if g == 1:
    print("✓ Already in SIMPLEST FORM")
else:
    print(f"Can reduce by {g}")
    N = N // g
    D = D // g

print()
print(f"Numerator:   {len(str(N))} digits")
print(f"Denominator: {len(str(D))} digits")
print()

print("="*70)
print("POSSIBLE PE ANSWER FORMATS FOR A FRACTION")
print("="*70)
print()

print("IF PE WANTS THE EXACT FRACTION AS A SINGLE INTEGER:")
print()

# 1. Modular representation: N mod D
print("1. N mod D (remainder when N divided by D):")
remainder = N % D
print(f"   N mod D = {remainder}")
print(f"   This is the FRACTIONAL PART represented as an integer")
print(f"   Digits: {len(str(remainder))}")
print()

# 2. Concatenation: combine N and D somehow
print("2. CONCATENATION approaches:")
print(f"   a) N||D (concatenate): too large ({len(str(N)) + len(str(D))} digits)")
print(f"   b) hash(N, D): non-standard")
print()

# 3. N + D
print("3. N + D (sum of numerator and denominator):")
sum_ND = N + D
print(f"   N + D = {sum_ND}")
print(f"   Digits: {len(str(sum_ND))}")
print()

# 4. N - D  
print("4. N - D (difference):")
diff_ND = N - D
print(f"   N - D = {diff_ND}")
print(f"   Digits: {len(str(diff_ND))}")
print()

# 5. XOR
print("5. N XOR D (bitwise XOR):")
xor_ND = N ^ D
print(f"   N XOR D = {xor_ND}")
print(f"   Digits: {len(str(xor_ND))}")
print()

print("="*70)
print("MODULAR REDUCTIONS (PE COMMONLY USES THESE)")
print("="*70)
print()

moduli = [10**9, 10**9 + 7, 10**9 + 9, 998244353, 10**8, 10**7]

for mod in moduli:
    print(f"N mod {mod:>12} = {N % mod}")
    print(f"D mod {mod:>12} = {D % mod}")
    print(f"(N-D) mod {mod:>12} = {(N - D) % mod}")
    print(f"(N+D) mod {mod:>12} = {(N + D) % mod}")
    print(f"(N*D) mod {mod:>12} = {(N * D) % mod}")
    print()

print("="*70)
print("CONTINUED FRACTION REPRESENTATION")
print("="*70)
print()

# For very large fractions, continued fraction is compact
def continued_fraction_first_terms(n, d, max_terms=10):
    terms = []
    for _ in range(max_terms):
        if d == 0:
            break
        q = n // d
        terms.append(q)
        n, d = d, n - q * d
    return terms

cf_terms = continued_fraction_first_terms(N, D, 20)
print(f"First 20 terms of continued fraction: {cf_terms}")
print()
print("If PE wanted continued fraction, they'd typically ask for:")
print(f"  - Sum of first k terms: {sum(cf_terms[:10])}")
print(f"  - Product of first k terms: {cf_terms[0] * cf_terms[1] if len(cf_terms) > 1 else cf_terms[0]}")
print()

print("="*70)
print("MOST LIKELY REPRESENTATIONS")
print("="*70)
print()

candidates = [
    ("N mod D (the fractional part)", N % D),
    ("N mod 10^9", N % (10**9)),
    ("D mod 10^9", D % (10**9)),
    ("(N+D) mod 10^9", (N + D) % (10**9)),
    ("(N-D) mod 10^9", (N - D) % (10**9)),
]

print("Top candidates:")
for i, (desc, val) in enumerate(candidates, 1):
    print(f"{i}. {desc:.<45} {val}")

print()
print("="*70)
print("CRITICAL QUESTION")
print("="*70)
print()
print("IF g(16) is truly N/D, PE must accept ONE of these:")
print("1. N mod D - the remainder (fractional part as integer)")
print("2. Some modular reduction (N mod 10^9, etc.)")
print("3. Some combination (N+D, N-D, etc.) possibly mod something")
print()
print("The problem statement says 'Find g(16)'.")
print("If g(16) = N/D, then mathematically g(16) IS the fraction.")
print("But PE needs an integer, so they must want a specific encoding.")

