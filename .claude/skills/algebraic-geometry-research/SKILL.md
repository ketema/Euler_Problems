---
name: algebraic-geometry-research
description: Rigorous mathematical research methodology for algebraic geometry emphasizing deep understanding, systematic learning, and research discipline over rapid computation
---

# Algebraic Geometry Research Discipline

## Core Constitutional Principle

**UNDERSTAND BEFORE COMPUTING**: Never proceed with calculation until you can answer precisely:

1. What does each symbol represent?
1. What is the domain/codomain of each function?
1. What are the units/dimensions of each term?
1. Why am I doing this calculation? What question does it answer?

**If you cannot answer these, STOP. Go back to definitions.**

## Phase 1: Meta-Cognitive Discipline (MOST IMPORTANT)

### The "I Don't Actually Understand This" Detector

Before claiming understanding of ANY concept, you MUST pass:

#### 1. The Definition Test

- Write formal definition from memory
- Provide 3 examples: trivial, typical, pathological
- Provide 2 non-examples (things that almost work but don't)
- Explain why definition is stated precisely as it is (what breaks if you change it?)

#### 2. The Dimensional Sanity Check

- Every term in equation must have same "mathematical dimension"
- Example: Cannot add point to vector in geometry
- Example: Cannot add element to ideal in algebra
- **HABIT**: Before manipulating any equation, label what type of object each term is

#### 3. The Simplest Case Test

- Can you work through statement when n=1? n=0?
- Can you verify for most trivial example imaginable?
- **PRINCIPLE**: If you can't do trivial case, you don't understand general case

#### 4. The "Explain to a Student" Test

- Write explanation as if teaching someone
- If using jargon, can you explain in more basic terms?
- Can you draw picture or diagram?
- Where would student get confused? Can you address that?

### Research Red Flags (STOP IMMEDIATELY IF YOU CATCH YOURSELF)

❌ Using definition you haven't verified you understand
❌ Manipulating symbols without knowing what they represent
❌ Skipping "trivial" cases because they seem obvious
❌ Proceeding when dimensional analysis doesn't check out
❌ Assuming something "should" work without verification
❌ Using advanced tools when basic tools haven't been tried
❌ Getting same type of error repeatedly (systematic error pattern)

## Phase 2: Foundational Methodology

### The Systematic Learning Protocol

For EVERY new concept in algebraic geometry:

#### Day 1: Definitions and Examples

- Write formal definition
- Identify every sub-concept it depends on (check: do you really understand those?)
- Compute 5 explicit examples with different characteristics
- Find 2 non-examples and understand exactly what fails
- Draw pictures/diagrams where possible

#### Day 2: Basic Properties

- What are immediate consequences of definition?
- What operations can you perform? (add, multiply, compose, etc.)
- What are the invariants? (things that don't change)
- What are natural symmetries?

#### Day 3: Relationships

- How does this relate to concepts you already know?
- Are there multiple equivalent definitions?
- Can you move between geometric and algebraic viewpoints?
- What's the simplest non-trivial theorem about this?

#### Day 4: Computation

- Work through 3-5 computational exercises by hand
- Identify patterns in computations
- What makes computations easy vs. hard?
- Where do you get stuck? (Those are your conceptual gaps)

#### Day 5: Reflection

- What is the "big idea" behind this concept?
- Why do mathematicians care about this?
- What questions does it answer?
- What new questions does it raise?

## Phase 3: Algebraic Geometry Learning Path

### Module 1: Polynomial Rings and Ideals (Week 1-2)

**Goal**: Understand algebraic side before adding geometry

**Core Questions to Answer**:

- What is a polynomial ring? (Not just "polynomials" - what's the ring structure?)
- What is an ideal? Why do we quotient by ideals?
- What does R/I represent geometrically?

**Mandatory Exercises**:

1. Describe all ideals of ℝ[x] explicitly
1. Compute k[x,y]/(xy, x²-y) step by step - what is this ring?
1. Show that (x², xy, y²) ≠ (x,y)² as ideals - find explicit element in one but not other
1. Build quotient k[x,y]/(y-x²) and understand it as functions on parabola

**Skill Check**:

- Can you determine if two ideals are equal?
- Can you compute in quotient rings?
- Do you understand why Spec(R) is defined as it is?

### Module 2: Affine Varieties (Week 3-4)

**Goal**: Connect algebra to geometry

**Core Questions**:

- What does V(I) mean geometrically?
- What does I(V) mean algebraically?
- Why isn't V(I(V)) always equal to V?

**Mandatory Exercises**:

1. For I = (x² + y² - 1) in ℝ[x,y], describe V(I) and compute I(V(I))
1. Find ideal I where V(I) is empty but I ≠ R
1. Describe all varieties in ℝ² that are finite sets of points
1. Show that V(I ∩ J) = V(I) ∪ V(J) with explicit examples

**Skill Check**:

- Can you move fluently between varieties and ideals?
- Do you understand why we work over algebraically closed fields?
- Can you visualize varieties in low dimensions?

### Module 3: Projective Varieties (Week 5-6)

**Goal**: Understand compactification and homogeneous coordinates

**Core Questions**:

- Why do we need projective space?
- What does "homogeneous" really mean?
- Why can't we just use affine space for everything?

**Mandatory Exercises**:

1. Describe ℙ¹(ℝ) and ℙ¹(ℂ) explicitly as sets
1. Map affine line into ℙ¹ and describe what's "added at infinity"
1. Take y = x² in ℝ² and find its projective closure in ℙ²
1. Show why (x, y, z) and (λx, λy, λz) define same point
1. Compute all intersection points of x² + y² = z² and x = y in ℙ²(ℂ)

**Skill Check**:

- Can you work comfortably with homogeneous coordinates?
- Do you understand Zariski topology on ℙⁿ?
- Can you compute intersections in projective space?

### Module 4: Dimension Theory (Week 7-8)

**Goal**: Understand what "dimension" means algebraically

**Core Questions**:

- What does it mean for variety to be d-dimensional?
- How does algebraic dimension relate to geometric dimension?
- What is Krull dimension?

**Mandatory Exercises**:

1. Compute dimensions of V(xy), V(x), V(x² + y² - 1) in ℝ²
1. Find all prime ideals of k[x]/(x²) and draw the Spec
1. Show that k[x,y]/(xy) has dimension 1 but is not irreducible
1. Build 3-dimensional variety in ℝ⁴

**Skill Check**:

- Can you determine dimension from ideal generators?
- Do you understand irreducibility?
- Can you relate chains of prime ideals to dimension?

### Module 5: Morphisms and Function Fields (Week 9-10)

**Goal**: Understand maps between varieties

**Core Questions**:

- What is morphism of varieties?
- Why do morphisms go one way but ring maps go the other?
- What is rational function?

**Mandatory Exercises**:

1. Describe all morphisms ℙ¹ → ℙ¹
1. Show that x ↦ (x, x²) defines ℝ → ℝ² as morphism
1. Compute pullback of f(x,y) = x under parabola map
1. Find function field of y² = x³ and understand it

**Skill Check**:

- Can you verify if map is morphism?
- Do you understand contravariant functor picture?
- Can you work with rational functions?

## Phase 4: Research Methodology

### The Problem-Solving Protocol

#### Step 1: Understanding (15-30 min)

- Restate problem in your own words
- Identify every definition involved - do you truly understand each?
- What is being asked? (Compute? Prove? Find example?)
- What am I given? What constraints exist?

#### Step 2: Exploration (30-60 min)

- Solve n=0, n=1 cases completely by hand
- Solve n=2 case completely by hand
- Look for patterns
- Form conjectures about general case
- Test conjectures on n=3

#### Step 3: Strategy (15-30 min)

- What tools might apply? (Don't use them yet - just list possibilities)
- What's simplest possible approach?
- What are conceptual obstacles?
- What sub-problems can I break this into?

#### Step 4: Attempt (variable)

- Start with simplest strategy
- Check dimensional consistency at each step
- Verify intermediate results on simple cases
- If stuck >15 minutes, go back to Step 2

#### Step 5: Verification (15-30 min)

- Does answer make sense dimensionally?
- Does it work for trivial cases?
- Does it pass sanity checks?
- Can I explain why this is the answer?

#### Step 6: Reflection (10-15 min)

- What was key insight?
- What mistakes did I make?
- What would I do differently next time?
- What did I learn about the mathematics?

### The "I'm Stuck" Protocol

When stuck, ask in order:

#### 1. Conceptual Understanding

- Do I actually understand what every symbol means?
- Have I verified the trivial case?
- Am I making unstated assumptions?

#### 2. Dimensional Analysis

- Does my equation make sense dimensionally?
- Am I adding/comparing incompatible objects?
- What type of object should my answer be?

#### 3. Simplification

- What's simplest version of this problem?
- Can I remove parameters/constraints to make it easier?
- Have I tried examples?

#### 4. Different Viewpoints

- Can I think geometrically instead of algebraically (or vice versa)?
- Is there dual formulation?
- What would this look like in coordinates?

#### 5. Tools and Techniques

- Am I using right tools? (Maybe I'm overcomplicating)
- What do I know that I haven't used yet?
- What would expert try first?

#### 6. Meta-Analysis

- Why am I stuck? (Conceptual gap? Computational difficulty? Wrong approach?)
- What would help me make progress?
- Should I step back and learn something first?

## Phase 5: Building Research Intuition

### Pattern Recognition Training

#### Exercise Type 1: Spot the Pattern

1. Compute Hilbert polynomials of several varieties
1. Look for relationships between degree, dimension, coefficients
1. Form conjectures
1. Test on new examples
1. Try to prove or refine

#### Exercise Type 2: Classify and Characterize

- "Find all quadric surfaces in ℙ³"
- "Classify all curves in ℙ² of degree 2"
- Forces systematic and complete thinking

#### Exercise Type 3: Generalization

1. Take theorem you know for curves
1. Try to generalize to surfaces
1. Where does proof break?
1. What new phenomena appear?

#### Exercise Type 4: Counter-Examples

- "Find variety that is X but not Y"
- Tests deep understanding of definitions
- Reveals why hypotheses in theorems matter

### Proof Technique Training

**Basic Proof Patterns to Master**:

1. **Direct Computation**
   - Write everything in coordinates
   - Compute explicitly
   - Useful for: verification, small cases, building intuition
1. **Dimension Counting**
   - Count parameters/degrees of freedom
   - Use intersections and linear systems
   - Useful for: existence, genericity arguments
1. **Diagram Chasing**
   - Draw commutative diagrams
   - Follow elements through maps
   - Useful for: sheaf cohomology, functorial arguments
1. **Reduction to Known Cases**
   - Show your problem is equivalent to one you know
   - Use standard forms (normalization, smoothness, etc.)
   - Useful for: avoiding reinventing wheels
1. **Contradiction and Contrapositive**
   - Assume the opposite
   - Derive impossibility
   - Useful for: uniqueness, non-existence

## Phase 6: Self-Assessment and Growth

### Weekly Self-Checks

Every week, assess:

- What new concepts did I learn?
- Can I define them precisely from memory?
- Can I compute examples without references?
- What mistakes did I make repeatedly?
- What gaps in understanding did I discover?
- What connections between concepts did I find?

### Monthly Milestones

**After 1 month**:

- Comfortable with affine varieties and ideals
- Can compute in quotient rings fluently
- Understand Hilbert's Nullstellensatz (statement and significance)
- Can solve basic problems without references

**After 2 months**:

- Comfortable with projective varieties
- Understand dimension theory
- Can work with morphisms
- Can read graduate-level AG papers (with effort)

**After 3 months**:

- Can formulate original conjectures
- Can prove simple statements independently
- Understand cohomological tools (at least Čech cohomology)
- Ready for research problems

### The Core Habits

**Daily Practice** (spend):

- 30 min: Reading and understanding definitions
- 60 min: Working through exercises by hand
- 30 min: Writing up solutions formally
- 15 min: Reflection and self-assessment

## Quality Standards

Your work should meet these standards:

- Every claim is either proved or cited
- Every definition is used precisely
- Every computation is verifiable
- Every example is fully worked out
- Every proof is clear and complete

## The Mindset

Remember:

- **Slow is fast**: Understanding deeply now saves time later
- **Simple is powerful**: Advanced tools often obscure simple truths
- **Examples are essential**: Abstraction without examples is empty
- **Questions matter**: The right question is half the answer
- **Mistakes are data**: Each error teaches you something
- **Rigor is liberating**: Precision eliminates confusion

## Concrete First Assignment

To test this methodology, begin with:

**Task**: Understand "scheme" (Spec of ring)

Instead of trying to understand abstractly:

1. Compute Spec(ℝ) explicitly - what are prime ideals?
1. Compute Spec(ℂ[x]) explicitly - what are prime ideals?
1. Compute Spec(ℤ) explicitly - what are prime ideals?
1. Compute Spec(ℝ[x]/(x²)) explicitly
1. For each case, understand Zariski topology
1. Draw pictures/diagrams of these spaces
1. ONLY THEN look at general definition of Spec(R)
1. See how general definition captures what you computed

**Success metric**: Can you explain Spec to someone who only knows rings and ideals, using your examples?

## Meta-Principle: Research is a Skill

Mathematics research is **NOT** about:

- ❌ Memorizing theorems
- ❌ Applying formulas quickly
- ❌ Getting answers fast

Mathematics research **IS** about:

- ✅ Understanding deeply: Know why, not just what
- ✅ Thinking systematically: Have methods for getting unstuck
- ✅ Being rigorous: Precision in thought and expression
- ✅ Building intuition: Geometric, algebraic, and computational
- ✅ Asking questions: Good questions drive discovery
- ✅ Communicating clearly: If you can't explain it, you don't get it

## Workflow Integration with Claude

When working on algebraic geometry with Claude:

1. **State Learning Goal**: Clearly identify concept to understand
1. **Definition Phase**: Have Claude write formal definition and explain each component
1. **Example Generation**: Request trivial, typical, and pathological examples
1. **Verification**: Work through examples by hand, verify understanding
1. **Dimensional Check**: Confirm all manipulations respect mathematical types
1. **Simplest Cases**: Always compute n=0, n=1 cases explicitly
1. **Reflection**: Discuss what was learned and identify remaining gaps

## Evidence of Understanding

Before moving to next topic, you must provide:

```
UNDERSTANDING EVIDENCE:
- [DEFINITION:concept] - Can state precisely from memory
- [EXAMPLES:3-trivial,typical,pathological] - Computed by hand
- [NON-EXAMPLES:2] - Identified what fails and why
- [TRIVIAL-CASE:verified] - n=0 or n=1 case worked completely
- [DIMENSIONAL-CHECK:passed] - All terms have consistent types
- [EXPLANATION:clear] - Can teach concept to someone else
```

## Anti-Patterns (Research Violations)

❌ **Computing before understanding** → STOP, answer the 4 core questions
❌ **Skipping trivial cases** → STOP, verify n=0 and n=1 first
❌ **Using undefined terms** → STOP, define all concepts precisely
❌ **Dimensional inconsistency** → STOP, check mathematical types
❌ **Assuming "obvious" facts** → STOP, verify explicitly
❌ **Rushing to advanced tools** → STOP, try basic approaches first
❌ **Pattern without proof** → STOP, verify pattern rigorously

## Violation Recovery

When research violation detected:

1. **STOP immediately** → Acknowledge what went wrong
1. **Identify principle broken** → "I violated 'Understand Before Computing' by…"
1. **Return to last valid state** → Go back to definitions/trivial cases
1. **Ask**: "Should I restart with proper discipline?"
1. **Document lesson** → What did this teach about the mathematics?

## Success Criteria Checklist

Before claiming understanding:

- [ ] Can state formal definition from memory
- [ ] Have computed 3 diverse examples by hand
- [ ] Have identified 2 clear non-examples
- [ ] Understand why definition is stated as it is
- [ ] Verified trivial cases (n=0, n=1)
- [ ] All dimensional checks pass
- [ ] Can explain concept to someone else
- [ ] Know what questions this concept answers
- [ ] Identified connections to previously learned material
- [ ] Completed mandatory exercises for module
- [ ] Can solve problems without references
- [ ] Have reflected on key insights and mistakes

## Remember

**Research quality = depth_of_understanding × rigor_of_verification**

**Fast computation without understanding is worthless**

**Taking time to understand deeply ENABLES faster progress later**

**Precision in thought eliminates confusion and false starts**

---

*This skill enforces rigorous mathematical research discipline for algebraic geometry, prioritizing deep understanding and systematic methodology over computational speed*
