{-----------------------------------------------------------------------------------------
Module name: URL
Made by:     Tomas Möre 2015

The url parser needs work

This library is mean to be imported qualified 
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}
module URL where

import Util 
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

type Scheme   = ByteString 
type Host     = ByteString
type Port     = Maybe ByteString
type Path     = Maybe ByteString
type Query    = Maybe ByteString
type Fragment = Maybe ByteString

type Rest     = ByteString


data URL = URL Scheme  Host Port Path Query Fragment

type URLAddedScheme = (Host -> Port -> Path-> Query -> Fragment -> URL)
type URLAddedHost   = (Port -> Path-> Query -> Fragment -> URL)
type URLAddedPort   = (Path -> Query -> Fragment -> URL)
type URLAddedPath   = (Query -> Fragment -> URL)
type URLAddedQuery  = (Fragment -> URL)


getScheme   (URL a _ _ _ _ _) = a
getHost     (URL _ a _ _ _ _) = a
getPort     (URL _ _ a _ _ _) = a
getPath     (URL _ _ _ a _ _) = a
getQuery    (URL _ _ _ _ a _) = a
getFragment (URL _ _ _ _ _ a) = a


schemeSeparator = BI.c2w ':'
portSeparator   = schemeSeparator
pathSeparator   = BI.c2w '/'
querySeparator  = BI.c2w '?'
fragmentSeparator = BI.c2w '#'

parseScheme:: ByteString -> Maybe (URLAddedScheme, Rest)
parseScheme ""    = Nothing
parseScheme strIn = let (tempScheme, temprest) = B.breakByte schemeSeparator strIn
                        build = URL $ B.copy tempScheme  
                    in case temprest of 
                            "" -> Nothing
                            _  -> Just (build, B.drop 2 temprest) 

parseHost:: (URLAddedScheme, Rest) -> Maybe (URLAddedHost, Maybe Rest)
parseHost (_, "")           = Nothing
parseHost (build, strIn)    = let   (tempHost,  temprest)   = B.breakByte portSeparator strIn
                                    (tempHost2, tempRest2)  = B.breakByte pathSeparator strIn

                                    maybeRest a = case a of 
                                                    "" -> Nothing
                                                    _  -> Just temprest
                               in case temprest of 
                                        "" ->  Just (build tempHost2, maybeRest tempRest2)
                                        _  ->  Just (build tempHost,  maybeRest temprest) -- No drop here because of efficient check on the next step

parsePort:: (URLAddedHost, Maybe Rest) -> (URLAddedPort, Maybe Rest)
parseProt (build, Nothing)      = (build Nothing, Nothing)
parsePort (build, Just strIn)   =   let firstChar    = B.head strIn
                                        (port, rest) = B.breakByte pathSeparator $ B.tail strIn -- the tail step from previous step is run here instead
                                        tail         = Just $ (B.tail rest)
                                    in if firstChar ==  portSeparator
                                        then (if rest == B.empty 
                                                then 
                                                    (build $ Just port, Nothing)
                                                else
                                                    (build $ Just port, tail))
                                        else
                                            (build Nothing, tail)

parsePath:: (URLAddedPort, Maybe Rest) -> (URLAddedPath, Maybe Rest)
parsePath (build, Nothing)    = (build Nothing, Nothing)
parsePath (build, Just strIn) = let (path, rest) = B.breakByte querySeparator strIn
                                in case rest of 
                                        "" -> (build $ Just path, Nothing)
                                        _  -> (build $ Just path, Just rest)

parseQuery :: (URLAddedPath, Maybe Rest) -> (URLAddedQuery, Maybe Rest)
parseQuery (build, Nothing)    = (build Nothing, Nothing)
parseQuery (build, Just strIn) = let (query, rest) = B.breakByte fragmentSeparator strIn
                                 in case rest of 
                                        "" -> (build $ Just query, Nothing)
                                        _  -> (build $ Just query, Just $ B.tail rest)

parseFragment :: (URLAddedQuery, Maybe Rest) -> URL 
parseFragment (build, Nothing)    = build Nothing
parseFragment (build, Just strIn) = build $ Just  strIn 


parse :: ByteString -> Maybe URL 
parse strIn = let requiredSteps =  parseScheme strIn >>= parseHost
              in case requiredSteps of 
                    Nothing   -> Nothing 
                    Just rest -> Just (parseFragment $ parseQuery $ parsePath $ parsePort rest)


toString :: URL ->  ByteString
toString (URL scheme  host port path query fragment) = 
        let justOrEmpty a = case a of 
                                Nothing -> ""
                                Just b  -> b
        in B.concat [ scheme
                    , "://"
                    , host
                    , justOrEmpty $ fmap (B.append ":") port
                    , justOrEmpty $ fmap (B.append "/") path
                    , justOrEmpty $ fmap (B.append "?") query
                    , justOrEmpty $ fmap (B.append "#") fragment]







