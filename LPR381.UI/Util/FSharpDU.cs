using Microsoft.FSharp.Core;
using Microsoft.FSharp.Reflection;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace LPR381.UI.Util
{
    public static class FSharpDu
    {
        // Unwrap F# option<T> in C#
        public static bool TrySome<T>(FSharpOption<T> opt, out T value)
        {
            if (FSharpOption<T>.get_IsSome(opt)) { value = opt.Value; return true; }
            value = default!;
            return false;
        }

        // Read DU case name + payloads; note the 3-arg overload and no tuple deconstruction.
        public static (string CaseName, object[] Fields) ReadUnion(object du)
        {
            // pass null for FSharpOption<BindingFlags>
            var uf = FSharpValue.GetUnionFields(du, du.GetType(), null);
            var uci = uf.Item1;      // UnionCaseInfo
            var fields = uf.Item2;   // object[]
            return (uci.Name, fields);
        }

        // Convert F# Map<string,double> (or any IEnumerable<KeyValuePair<string,double>>) to Dictionary
        public static IDictionary<string, double> ToDict(object mapLike)
        {
            if (mapLike is IEnumerable<KeyValuePair<string, double>> kvs)
                return kvs.ToDictionary(k => k.Key, v => v.Value);

            var dict = new Dictionary<string, double>();
            foreach (var x in (IEnumerable)mapLike)
            {
                var t = x.GetType();
                var k = (string)t.GetProperty("Key", BindingFlags.Public | BindingFlags.Instance)!.GetValue(x)!;
                var v = (double)t.GetProperty("Value", BindingFlags.Public | BindingFlags.Instance)!.GetValue(x)!;
                dict[k] = v;
            }
            return dict;
        }
    }
}
