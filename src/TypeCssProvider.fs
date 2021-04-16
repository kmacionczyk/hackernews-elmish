[<AutoOpen>]
module TypeCssProvider

open Zanaptak.TypedCssClasses

// A "Bulma" type pointing at a remote Bulma CSS file.
type Bulma = CssClasses<"https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css", Naming.PascalCase>

// A "FA" type pointing at a remote FontAwesome CSS file.
type FA = CssClasses<"https://use.fontawesome.com/releases/v5.8.1/css/all.css", Naming.PascalCase>