module WBPS.Core.Registration.FileScheme.Directories (
  Accounts,
  Account,
  AccountName,
) where

import Path (
  Abs,
  Dir,
  Path,
  Rel,
 )

type Accounts = Path Abs Dir

type Account = Path Abs Dir

type AccountName = Path Rel Dir
