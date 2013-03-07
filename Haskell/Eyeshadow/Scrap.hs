
diagnoseInvalidNumber
  :: (Monad m)
  => SourceFileSpecification
  -> SourceSpan
  -> Producer m (Either Diagnostic b)
diagnoseInvalidNumber file span = yield $ Left $
  Diagnostic {
      diagnosticHeadline = "Invalid numeric literal",
      diagnosticDescription =
        T.concat
          ["A potential numeric literal, lexically, is any sequence of ",
           "digit or constituent characters which is started by a digit ",
           "character.  This is to allow for floating-point syntaxes and ",
           "other creative notations.  However, this particular literal ",
           "did not obey the syntactic rules of any of the types it might ",
           "otherwise have inhabited."],
      diagnosticDetails =
        [("Offending numeric literal", file, span)]
    }

