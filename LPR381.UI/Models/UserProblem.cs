using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LPR381.Core;

namespace LPR381.UI.Models
{
    /// Raw lines the user typed in the UI.
    public sealed class UserProblem
    {
        public string ObjectiveLine { get; init; } = "";   // e.g. "max z = 3x1 + 2x2"
        public string[] Constraints { get; init; } = [];   // e.g. "2x1 + 3x2 <= 10"
        public string? DecisionVariablesLine { get; init; } // optional (e.g. "x1, x2 >= 0" or "x1 int")
        public string IntMode { get; init; } = "continuous";
        public Dictionary<string, SignRestriction>? VariableSignRestrictions { get; init; }
        public Dictionary<string, IntRestriction>? VariableIntRestrictions { get; init; }
    }

}
