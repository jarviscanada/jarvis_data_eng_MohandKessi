import React from 'react'
import './NavBar.scss'
import { NavLink } from 'react-router-dom'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faAddressBook as dashboardIcon } from '@fortawesome/free-solid-svg-icons';

function NavBar() {
  return (
    <nav className="page-navigation">
		    <NavLink to="/" className="page-navigation-header"></NavLink>    
		    <NavLink to="/traders" className="page-navigation-item">
		        <FontAwesomeIcon icon={ dashboardIcon } />
		    </NavLink>    
    </nav>
  )
}

export default NavBar