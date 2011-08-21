<?php

class StringChunker
{
    public function chunk( $string, $chunkSize )
    {
        if(strlen($string) < $chunkSize)
        {
            throw new Exception('Chunk Size > length');
        }

        $arr = str_split($string);
        $chunks = array();

        if( sizeof($arr) > $chunkSize)
        { 
            foreach( range(0, (sizeof($arr) - ($chunkSize + 1)) ) as $index )
            {
               array_push($chunks, array_slice($arr, $index, $index + $chunkSize)); 
            }
        }
        else{
            array_push($chunks, array_slice($arr, 0, $chunkSize));
        }
        return $chunks;
    }
}
