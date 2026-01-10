structure ModSyn =
  ModSyn (structure Global = Global
          structure Names' = Names
          structure Origins = Origins
          structure Whnf = Whnf
          structure Strict = Strict
          structure IntTree = IntRedBlackTree
          structure HashTable = StringHashTable);
