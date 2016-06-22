using FsCheck;
using FsCheck.Xunit;
using System;
using System.Collections.Generic;
using System.Linq;

namespace CaesarCsharp
{
    public abstract class Nachricht<T>
    {
        private string _message;

        public override string ToString()
        {
            return _message;
        }

        public override bool Equals(object obj)
        {
            var vgl = obj as Nachricht<T>;
            if (vgl == null) return false;
            return vgl._message == this._message;
        }

        public static bool operator ==(Nachricht<T> a, Nachricht<T> b)
        {
            if (object.Equals(null,a))
                return object.Equals(null,b);

            return a.Equals(b);
        }

        public static bool operator !=(Nachricht<T> a, Nachricht<T> b)
        {
            return !a.Equals(b);
        }

        public override int GetHashCode()
        {
            return _message.GetHashCode();
        }

        protected Nachricht(string input)
        {
            _message = Normalize(input);
        }

        private static string Normalize(string input)
        {
            if (String.IsNullOrEmpty(input))
                return "";
            var chars =
                input
                .ToUpper()
                .Trim()
                .Where(c => (int)c >= (int)'A' && (int)c <= (int)'Z')
                .ToArray();
            return new string(chars);
        }
    }

    public class Klartext : Nachricht<Klartext>
    {
        public Klartext(string input) : base(input)
        {
        }
    }

    public class Verschlüsselt : Nachricht<Verschlüsselt>
    {
        public Verschlüsselt(string input) : base(input)
        {
        }
    }

    public static class Caesar
    {
        static string mapChars(IEnumerable<Char> chars, Func<Char,Char> f)
        {
            return new string(
                chars.Select(f)
                .ToArray());
        }

        static int Mod(int n, int m)
        {
            var r = n % m;
            if (r < 0) return m + r;
            return r;
        }

        static Func<char, char> Translate(int schlüssel)
        {
            return c =>
            {
                var n = (int)c - (int)'A';
                return (char)(Mod(n + schlüssel, 26) + (int)'A');
            };
        }

        public static Verschlüsselt Verschlüsseln(int schlüssel, Klartext klartext)
        {
            var translated = mapChars(klartext.ToString(), Translate(schlüssel));
            return new Verschlüsselt(translated);
        }

        public static Klartext Entschlüsseln(int schlüssel, Verschlüsselt verschlüsselt)
        {
            var translated = mapChars(verschlüsselt.ToString(), Translate(-schlüssel));
            return new Klartext(translated);
        }
    }

    /// <summary>
    /// das Property-Attribut in XUnit scheint die Klasse selbst zu finden
    /// ansonsten muss in Xunit der Typ mit im Attribut übergeben werden
    /// (kein Arb.Register müsste in ein Test-Setup)
    /// </summary>
    public class KlartextArb : Arbitrary<Klartext>
    {
        public override Gen<Klartext> Generator
        {
            get
            {
                return
                    Gen.Elements(Enumerable.Range((int)'A', 26).Select(i => (char)(i + (int)'A')))
                    .NonEmptyListOf()
                    .Select(cs => new string(cs.ToArray()))
                    .Select(s => new Klartext(s));
            }
        }

        public override IEnumerable<Klartext> Shrinker(Klartext text)
        {
            var s = text.ToString();
            if (s.Length < 2) return new Klartext[] { };

            var mid = s.Length / 2;
            return new[]
            {
                new Klartext(s.Substring(0,mid)),
                new Klartext(s.Substring(mid))
            };
        }
    }

    public class Tests
    {

        /// <summary>
        /// Mit "Label" muss man ein Property zurückgeben
        /// </summary>
        [Property]
        public Property Inverse(int schlüssel, Klartext nachricht)
        {
            var enc = Caesar.Verschlüsseln(schlüssel, nachricht);
            var ent = Caesar.Entschlüsseln(schlüssel, enc);
            return (nachricht == ent).Label(string.Format("{0} => {1} => {2}", nachricht, enc, ent));
            
        }

        /// <summary>
        /// ansonsten ist bool ok
        /// </summary>
        [Property]
        public bool LängeBleibtGleich(int schlüssel, Klartext nachricht)
        {
            var enc = Caesar.Verschlüsseln(schlüssel, nachricht);
            return nachricht.ToString().Length == enc.ToString().Length;
        }
    }
}
