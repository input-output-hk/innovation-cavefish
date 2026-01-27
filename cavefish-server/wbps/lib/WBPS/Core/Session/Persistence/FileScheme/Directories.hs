module WBPS.Core.Session.Persistence.FileScheme.Directories (
  Sessions,
  Session,
  SessionName,
) where

import Path (
  Abs,
  Dir,
  Path,
  Rel,
 )

type Sessions = Path Abs Dir

type Session = Path Abs Dir

type SessionName = Path Rel Dir
