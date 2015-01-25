{-----------------------------------------------------------------------------------------
Module name: URL
Made by:     Tomas Möre 2015


This library is mean to be imported qualified 
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}


import Util 
import Data.Maybe
import qualified Data.Bytestring as B
import qualified Data.Bytestring.Internal as BI

type Scheme   = Bytestring 
type Host     = Bytestring
type Port     = Just Bytestring
type Path     = Just Bytestring
type Query    = Just Bytestring
type Fragment = Just Bytestring

type Rest     = Bytestring


data URL = URL Scheme  Host Port Path Query Fragment

type URLAddedScheme = (Host -> Port -> Path-> Query -> Fragment -> URL)
type URLAddedHost   = (Port -> Path-> Query -> Fragment -> URL)
type URLAddedPort   = (Path -> Query -> Fragment -> URL)
type URLAddedPath   = (Query -> Fragment -> URL)
type URLAddedQuery  = (Fragment -> URL)


getScheme   (a,_,_,_,_,_) = a
getHost     (_,a,_,_,_,_) = a
getPort     (_,_,a,_,_,_) = a
getPath     (_,_,_,a,_,_) = a
getQuery    (_,_,_,_,a,_) = a
getFragment (_,_,_,_,_,a) = a


schemeSeparator = BI.c2w ':'
portSeparator   = schemeSeparator
pathSeparator   = BI.c2w '/'
querySeparator  = BI.c2w '?'
fragmentSeparator = BI.c2w '#'

parseScheme:: Bytestring -> Maybe (URLAddedScheme, Rest)
parseScheme ""    = Nothing
parseScheme strIn = let (tempScheme, temprest) = B.breakByte schemeSeparator strIn
                        build = URL $ B.copy tempScheme  
                    in case temprest of 
                            "" -> Nothing
                            _  -> Just (build, B.drop 2 temprest) 

parseHost:: (URLAddedScheme, Rest) -> Maybe (URLAddedHost, Maybe Rest)
parseHost (_, "")     		= Nothing
parseHost (build, strIn) = let 	(tempHost,  temprest)   = B.breakByte portSeparator strIn
                      			(tempHost2, tempRest2)  = B.breakByte pathSeparator strIn

                      			maybeRest a = case a of 
                      							"" -> Nothing
                      							_  -> Just temprest
	                  		in case temprest of 
	                        		"" ->  (build tempHost2, maybeRest tempRest2)
	                       			_  ->  (build tempHost,  maybeRest temprest) -- No drop here because of efficient check on the next step

parsePort:: (URLAddedHost, Maybe Rest) -> (URLAddedPort, Maybe Rest)
parseProt (build, Nothing) 		= (build "", Nothing)
parsePort (build, Just strIn)   = 	let firstChar    = B.head strIn
								 	  	(port, rest) = B.breakByte pathSeparator $ B.tail strIn -- the tail step from previous step is run here instead 
								 	in case firstChar of 
								 		portSeparator -> case rest of 
								 							"" -> (build port, Nothing)
								 							_  -> (build port, Just $ B.tail rest)
								 		_  			  -> (build "", Just $ B.drop)

parsePath:: (URLAddedPort, Maybe Rest) -> (URLAddedPath, Maybe Rest)
parsePath (build, Nothing) 	  = (build "", Nothing)
parsePath (build, Just strIn) = let (path, rest) = B.breakByte querySeparator strIn
								in case rest of 
										"" -> (build path, Nothing)
										_  -> (build path, Just rest)

parseQuery :: (URLAddedPath, Maybe Rest) -> (URLAddedQuery, Maybe Rest)
parseQuery (build, Nothing)	   = (build "", Nothing)
parseQuery (build, Just strIn) = let (query, rest) = B.breakByte fragmentSeparator strIn
								 in case rest of 
								 		"" -> (build query, Nothing)
								 		_  -> (build query, Just $ B.tail rest)

parseFragment :: (URLAddedQuery, Maybe Rest) -> URI 
parseFragment (build, Nothing) = (build "", Nothing)
parseFragment (build, Just strIn) = build strIn 


parse :: Bytestring -> Maybe URL 
parse strIn = let requiredSteps = strIn >>= parseScheme >>= parseHost
			  in case requiredSteps of 
			  		Nothing   -> Nothing 
			  		Just rest -> parseFragment . parseQuery . parsePath . parsePort rest


toString :: URL ->  Bytestring
toString (URL scheme  host port path query fragment) = 
		let justOrEmpty a = case a of 
								Nothing -> ""
								Just b  -> b
		in B.Concat [ scheme
					, "://"
					, host
				 	, justOrEmpty $ fmap (B.append ":") port
				 	, justOrEmpty $ fmap (B.append "/") path
				 	, justOrEmpty $ fmap (B.append "?") query
				 	, justOrEmpty $ fmap (B.append "#") fragment]







