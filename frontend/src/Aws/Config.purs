module Aws.Config where

type AwsConfig = forall c. { region :: String | c }
