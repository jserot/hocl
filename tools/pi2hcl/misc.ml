(**********************************************************************)
(*                                                                    *)
(*              This file is part of the HOCL package                 *)
(*                                                                    *)
(*  Copyright (c) 2019-present, Jocelyn SEROT (jocelyn.serot@uca.fr). *)
(*                     All rights reserved.                           *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

let replace_suffix ?(sep='.') s f =
    let i = String.rindex f sep in
    String.sub f 0 (i+1) ^ s 

let string_of_list f sep l =
  l |> List.map f |> String.concat sep

let id x = x
