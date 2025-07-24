// Imports we need for routing, provided in the react-router-dom library
import { BrowserRouter, Route, Routes } from 'react-router-dom';
// We have to import all components to be able to use them.
// Currently, we should only have the Dashboard component
import Dashboard from './page/Dashboard/Dashboard';
// Initialization of Router Component
export default function Router() {
        return (
            <BrowserRouter>
                <Routes>
                    <Route exact path="/" element={<Dashboard />} />
                    <Route exact path="/dashboard" element={<Dashboard />} />
               </Routes>
            </BrowserRouter>
        )
    }