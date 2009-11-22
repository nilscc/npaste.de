{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Menu
    ( menuHsp
    , LoggedIn (..)
    ) where

import HSP
import Users.State.User

data LoggedIn = LoggedInAs User
              | NotLoggedIn


menuHsp :: LoggedIn -> HSP XML
menuHsp NotLoggedIn =
    <dl id="menu">
        <dt>Paste!</dt>
        <dd>
            <ul>
                <li><a href="/">Add a new paste.</a></li>
                <li><a href="/?view=recent">View recent pastes.</a></li>
            </ul>
        </dd>

        <dt>Users</dt>
        <dd><ul>
            <li><a href="/?do=login">Login</a> or <a href="/?do=register">register</a>.</li>
            <li>Why register? Read the <a href="/?view=faq">FAQ</a></li>
        </ul></dd>
    </dl>

menuHsp (LoggedInAs user) =
    <dl id="menu">
        <dt>Paste!</dt>
        <dd>
            <p>Welcome back, <% userLogin user %>!</p>
            <ul>
                <li><a href="/">Add a new paste.</a></li>
                <li><a href="/?view=recent">View recent pastes.</a></li>
                <li><a href="/?view=recent">View your pastes.</a></li>
            </ul>
        </dd>
    </dl>
