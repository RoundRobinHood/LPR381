# Sensitivity Analysis - Variable and Constraint Analysis Functionality

## Overview

The Sensitivity Analysis tab now includes comprehensive functionality for analyzing and simulating changes to variables and constraints in linear programming problems. This implementation provides users with the ability to:

1. **Display Ranges**: Show allowable ranges for objective coefficients and RHS values
2. **Apply Changes**: Simulate changes to variables and constraints
3. **Validate Changes**: Check if changes are within allowable ranges
4. **Analyze Impact**: Understand the effects of changes on optimality and feasibility

## Features Implemented

### 1. **Non-Basic Variable Analysis** (`SA_NonBasic_ApplyChange_Click`)

**Purpose**: Applies and analyzes changes to non-basic variable objective coefficients.

**Functionality**:
- Validates user input for new coefficient values
- Checks if new values are within allowable ranges
- Provides comprehensive change analysis
- Warns about potential optimality issues

**Output Format**:
```
NON-BASIC VARIABLE CHANGE ANALYSIS:
=====================================
Variable: x1
Current Coefficient: 3.000000
New Coefficient: 4.000000
Change: 1.000000
Allowable Range: [2.500000, 5.000000]

✓ New value is within the allowable range.
The current solution remains optimal with this change.
```

**Validation**:
- Input validation for numeric values
- Range checking against sensitivity analysis results
- Clear warnings for out-of-range values

### 2. **Basic Variable Analysis** (`SA_Basic_ApplyChange_Click`)

**Purpose**: Applies and analyzes changes to basic variable objective coefficients.

**Functionality**:
- Similar to non-basic variables but with stricter validation
- Emphasizes the more restrictive nature of basic variable changes
- Provides educational content about basis structure implications

**Key Differences from Non-Basic**:
- Basic variables have more restrictive ranges
- Changes may affect the basis structure
- More critical for maintaining optimality

**Output Format**:
```
BASIC VARIABLE CHANGE ANALYSIS:
===============================
Variable: x2
Current Coefficient: 2.000000
New Coefficient: 2.500000
Change: 0.500000
Allowable Range: [1.800000, 2.200000]

⚠ WARNING: New value is outside the allowable range!
This change may:
- Make the current solution infeasible
- Require re-solving the problem
- Change the optimal solution
- Affect the basis structure
```

### 3. **Column Variable Analysis** (`SA_Column_ApplyChange_Click`)

**Purpose**: Analyzes changes to variables from a column perspective.

**Functionality**:
- Examines the impact of coefficient changes on entire columns
- Focuses on reduced costs and column structure
- Provides insights into how changes affect the overall problem

**Educational Value**:
- Explains column analysis concepts
- Shows how changes affect reduced costs
- Demonstrates the relationship between variables and constraints

### 4. **RHS Constraint Analysis** (`SA_RHS_ApplyChange_Click`)

**Purpose**: Applies and analyzes changes to constraint right-hand-side values.

**Functionality**:
- Validates new RHS values against allowable ranges
- Calculates estimated objective value changes using shadow prices
- Provides economic interpretation of changes
- Warns about feasibility issues

**Key Features**:
- **Shadow Price Integration**: Uses shadow prices to estimate objective changes
- **Range Validation**: Checks if new values maintain feasibility
- **Economic Analysis**: Shows the cost/benefit of constraint changes

**Output Format**:
```
RHS CONSTRAINT CHANGE ANALYSIS:
=================================
Constraint Index: 1
Current RHS: 25.000000
New RHS: 30.000000
Change: 5.000000
Allowable Range: [20.000000, 35.000000]
Shadow Price: 1.250000

✓ New RHS value is within the allowable range.
The current solution remains feasible with this change.
Estimated Objective Change: 6.250000
Shadow Price Interpretation: Positive shadow price - increasing RHS improves objective value
```

## Technical Implementation

### **Input Validation**
- **Numeric Parsing**: Robust parsing of user input with error handling
- **Range Checking**: Validation against sensitivity analysis results
- **User Feedback**: Clear error messages and guidance

### **Sensitivity Analysis Integration**
- **Context Management**: Uses existing sensitivity analysis context
- **Range Calculations**: Leverages F# core sensitivity analysis functions
- **Shadow Price Access**: Integrates with existing shadow price functionality

### **Error Handling**
- **Comprehensive Try-Catch**: Robust exception handling throughout
- **User-Friendly Messages**: Clear explanations of errors and solutions
- **Graceful Degradation**: Continues operation even with partial failures

### **UI Integration**
- **Real-Time Updates**: Immediate feedback on all operations
- **Status Messages**: Clear indication of operation progress and results
- **Input Validation**: Prevents invalid operations before execution

## Usage Workflow

### **Step 1: Display Ranges**
1. Select a variable or constraint from the dropdown
2. Click "Display Range" to see allowable values
3. Review the sensitivity analysis results

### **Step 2: Apply Changes**
1. Enter a new value in the input field
2. Click "Apply Change" to simulate the modification
3. Review the comprehensive analysis results

### **Step 3: Understand Impact**
1. Check if the new value is within allowable ranges
2. Review warnings about potential issues
3. Understand the implications for optimality and feasibility

## Educational Benefits

### **Learning Objectives**
- **Sensitivity Analysis**: Understanding how changes affect solutions
- **Range Concepts**: Learning about allowable parameter ranges
- **Shadow Prices**: Economic interpretation of constraint changes
- **Basis Structure**: Understanding how changes affect the simplex basis

### **Practical Applications**
- **Parameter Tuning**: Adjusting model parameters for different scenarios
- **What-If Analysis**: Exploring the impact of changes without re-solving
- **Model Validation**: Checking if parameter changes are reasonable
- **Decision Support**: Understanding the sensitivity of solutions to changes

## Error Scenarios and Handling

### **Common Issues**
- **Missing Context**: Clear message when no sensitivity analysis is available
- **Invalid Input**: Helpful guidance for numeric input requirements
- **Out-of-Range Values**: Warnings about potential optimality/feasibility issues
- **Missing Variables**: Clear identification of variable selection problems

### **User Guidance**
- **Actionable Messages**: Clear instructions on what to do next
- **Range Information**: Display of allowable ranges for reference
- **Impact Assessment**: Explanation of what changes mean for the solution
- **Recovery Options**: How to resolve different types of errors

## Performance Considerations

### **Efficient Operations**
- **Cached Context**: Reuses existing sensitivity analysis results
- **Minimal Recalculation**: Avoids unnecessary recomputation
- **Responsive UI**: Immediate feedback on all operations

### **Memory Management**
- **StringBuilder Usage**: Efficient string construction for large outputs
- **Context Reuse**: Minimizes memory allocation for repeated operations
- **Cleanup**: Proper disposal of temporary objects

## Future Enhancements

### **Potential Improvements**
- **Batch Operations**: Apply multiple changes simultaneously
- **Visual Feedback**: Charts and graphs showing change impacts
- **Export Functionality**: Save change analysis results to files
- **Advanced Validation**: More sophisticated range checking algorithms

### **Integration Opportunities**
- **Report Generation**: Create comprehensive change analysis reports
- **Scenario Management**: Save and restore different parameter configurations
- **Performance Metrics**: Timing and resource usage for change operations
- **Educational Modules**: Interactive tutorials on sensitivity analysis

## Conclusion

The variable and constraint analysis functionality provides users with powerful tools to:

1. **Understand Sensitivity**: See how changes affect optimal solutions
2. **Validate Changes**: Ensure modifications are within acceptable ranges
3. **Analyze Impact**: Understand the economic and mathematical implications
4. **Learn Concepts**: Gain practical understanding of sensitivity analysis

This implementation makes the LPR381 solver an excellent tool for both educational and practical linear programming applications, with particular emphasis on understanding how changes to problem parameters affect solutions and optimality.
