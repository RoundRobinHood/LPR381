# Dual Programming Sensitivity Analysis

## Overview

The Sensitivity Analysis tab now includes comprehensive functionality for analyzing dual programming problems and verifying duality relationships. This implementation provides users with the ability to:

1. **Solve Dual Problems**: Automatically generate and solve the dual formulation of a linear programming problem
2. **Verify Duality**: Check whether strong or weak duality holds between primal and dual problems
3. **Analyze Results**: Get detailed information about dual solutions and duality relationships

## Features Implemented

### 1. Dual Problem Solving (`SA_Duality_Solve_Click`)

**Purpose**: Generates and solves the dual formulation of the current primal problem.

**What it does**:
- Retrieves the current sensitivity analysis context
- Generates the dual formulation using the F# core's `DualFormulation` class
- Solves the dual problem using the `RevisedDualSimplex` algorithm
- Displays comprehensive results including:
  - Solution status (Optimal, Unbounded, or Infeasible)
  - Dual objective value
  - Individual dual variable values (shadow prices)

**Output Format**:
```
DUAL PROBLEM SOLUTION:
Status: Optimal
Dual Objective Value: 12.000000

Dual Variable Values:
  y1 = 1.000000
  y2 = 1.000000
```

### 2. Duality Verification (`SA_Duality_Verify_Click`)

**Purpose**: Verifies whether strong or weak duality holds between the primal and dual problems.

**What it does**:
- Solves both the primal and dual problems
- Compares their objective values
- Determines the type of duality relationship
- Provides detailed analysis and interpretation

**Duality Types**:

#### Strong Duality ✓
- **Condition**: Primal and dual objective values are equal (within numerical tolerance)
- **Meaning**: Both problems have the same optimal value
- **Output**: 
```
✓ STRONG DUALITY CONFIRMED
  Primal Objective: 12.000000
  Dual Objective:   12.000000
  Difference:       0.00E+00

Strong duality means the primal and dual problems
have the same optimal objective value.
```

#### Weak Duality ⚠
- **Condition**: Primal and dual objective values differ
- **Meaning**: Duality gap exists, but provides bounds
- **Output**:
```
⚠ WEAK DUALITY DETECTED
  Primal Objective: 12.000000
  Dual Objective:   11.500000
  Difference:       5.00E-01

Weak duality means the primal and dual problems
have different optimal objective values, but the
duality gap provides bounds on the solution.
```

#### No Duality ❌
- **Condition**: Problems are infeasible or unbounded
- **Meaning**: No meaningful duality relationship
- **Output**:
```
❌ NO DUALITY RELATIONSHIP
  Reason: Primal infeasible, dual unbounded

This indicates a problem with the formulation
or the solving process.
```

## Technical Implementation

### F# Core Integration
The implementation leverages the existing F# core functionality:

- **`DualFormulation`**: Automatically generates dual formulations
- **`RevisedDualSimplex`**: Solves both primal and dual problems
- **`RelaxedSimplexSensitivityContext`**: Provides sensitivity analysis context
- **`VerifyDuality`**: Built-in duality verification method

### C# UI Layer
The C# UI layer provides:

- **Event Handlers**: Connected to UI buttons in MainWindow.axaml
- **Error Handling**: Comprehensive try-catch blocks with user-friendly messages
- **Result Display**: Formatted output in the sensitivity analysis display area
- **Status Updates**: Real-time feedback on operation progress

## Usage Instructions

### Prerequisites
1. **Solve a Problem**: First solve a linear programming problem using Primal Simplex or Revised Simplex
2. **Navigate to Sensitivity Analysis**: Go to Results → Sensitivity Analysis tab
3. **Ensure Solution Exists**: The sensitivity analysis context must be available

### Step-by-Step Process

#### Step 1: Display Dual Formulation
1. Click the **"Apply Dual"** button to generate the dual formulation
2. Review the dual problem structure in the display area

#### Step 2: Solve Dual Problem
1. Click the **"Solve Dual"** button
2. Wait for the dual problem to be solved
3. Review the dual solution results

#### Step 3: Verify Duality
1. Click the **"Verify Duality"** button
2. Review the duality verification results
3. Understand the type of duality relationship

## Example Problem

Consider the standard problem:
```
Maximize: 3x + 2y
Subject to: x + y ≤ 4
           2x + y ≤ 6
           x, y ≥ 0
```

**Dual Formulation**:
```
Minimize: 4y₁ + 6y₂
Subject to: y₁ + 2y₂ ≥ 3
           y₁ + y₂ ≥ 2
           y₁, y₂ ≥ 0
```

**Expected Results**:
- **Strong Duality**: Both problems should have objective value 12
- **Dual Variables**: y₁ = 1, y₂ = 1 (shadow prices)
- **Verification**: Strong duality confirmed

## Error Handling

The implementation includes comprehensive error handling:

- **Missing Context**: Clear message when no sensitivity analysis context exists
- **Solver Errors**: Detailed error messages from the F# core
- **UI Errors**: Graceful fallbacks for missing UI controls
- **Exception Handling**: Try-catch blocks with user-friendly error messages

## Limitations and Considerations

### Current Limitations
- **Algorithm Dependency**: Only works with Primal Simplex and Revised Simplex results
- **Problem Types**: Limited to continuous linear programming problems
- **Numerical Precision**: Uses 1e-6 tolerance for duality verification

### Future Enhancements
- **Additional Solvers**: Extend to Branch and Bound and Cutting Plane algorithms
- **Integer Problems**: Handle mixed-integer programming problems
- **Advanced Analysis**: Include sensitivity ranges for dual variables
- **Export Functionality**: Save duality analysis results to files

## Testing

The functionality can be tested using the existing F# test files:

1. **`TestDuality.fsx`**: Tests basic duality relationships
2. **`SensitivityExample.fsx`**: Demonstrates complete sensitivity analysis
3. **UI Testing**: Use the Avalonia UI to test the new buttons

## Performance Considerations

- **Dual Solving**: Automatically triggered when needed
- **Memory Usage**: Efficient handling of large problem matrices
- **Numerical Stability**: Uses MathNet.Numerics for robust calculations
- **User Experience**: Non-blocking operations with progress feedback

## Conclusion

The dual programming sensitivity analysis functionality provides users with powerful tools to:

1. **Understand Duality**: See how primal and dual problems relate
2. **Verify Solutions**: Confirm mathematical correctness of results
3. **Analyze Relationships**: Explore the duality gap and its implications
4. **Educational Value**: Learn about linear programming duality concepts

This implementation enhances the LPR381 solver by providing comprehensive duality analysis capabilities, making it an excellent tool for both educational and practical linear programming applications.
