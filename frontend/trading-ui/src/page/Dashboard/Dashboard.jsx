import React from 'react'
import './Dashboard.scss'
// Import NavBar below
import NavBar from '../../component/NavBar/NavBar'

function Dashboard(props) {
  return (
    <div className="dashboard">
				{/* Include NavBar below */}
        <NavBar />
        <div className="dashboard-content">Dashboard Content</div>
    </div>
  )
}

export default Dashboard