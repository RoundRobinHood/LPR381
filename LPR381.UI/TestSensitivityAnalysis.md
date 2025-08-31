# Sensitivity Analysis Implementation

## Features Added

### 1. Variable Analysis
- **Basic Variables**: Dropdown populated with variables in the current basis
- **Non-Basic Variables**: Dropdown populated with variables not in the current basis  
- **Variable Column Analysis**: Dropdown with all variables for coefficient range analysis

### 2. Constraint Analysis
- **RHS Range Analysis**: Shows allowable range for right-hand-side values
- **Shadow Prices**: Displays dual prices and their economic interpretation

### 3. Duality Analysis
- **Dual Formulation**: Generates and displays the dual problem
- **Duality Verification**: Framework for verifying strong/weak duality

## How to Use

1. **Solve a Problem**: First solve a linear programming problem using Primal Simplex or Revised Simplex
2. **Navigate to Sensitivity Analysis**: Go to Results → Sensitivity Analysis tab
3. **Select Variables/Constraints**: Use the dropdowns to select specific variables or constraints
4. **Display Ranges**: Click "Display Range" buttons to see allowable parameter ranges
5. **View Shadow Prices**: Click "Display Shadow Prices" to see dual values
6. **Explore Duality**: Use the Duality tab to see the dual formulation

## Technical Implementation

- Uses `RelaxedSimplexSensitivityContext` from F# core for calculations
- Automatically populates dropdowns after solving
- Displays ranges in format `[lower_bound, upper_bound]`
- Shadow prices include economic interpretation
- Only works with continuous LP problems (no integer restrictions)

## Example Usage

1. Enter problem: `max 3x + 2y` subject to `x + y <= 4`, `x <= 2`, `y <= 3`
2. Solve using Primal Simplex
3. Go to Sensitivity Analysis → Variable Analysis
4. Select variable from Basic Variables dropdown
5. Click "Display Range" to see coefficient range
6. Go to Constraint Analysis → Shadow Prices
7. Click "Display Shadow Prices" to see dual values

## Limitations

- Variable change simulation not implemented (shows placeholder message)
- RHS change simulation not implemented (shows placeholder message)  
- Add new activity/constraint features not implemented
- Dual solving and verification not fully implemented
- Only works with Primal Simplex and Revised Simplex algorithms