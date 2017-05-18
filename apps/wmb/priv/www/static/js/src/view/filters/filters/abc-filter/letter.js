import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { ListItem, Avatar } from 'react-toolbox';

const LetterView = ({ onClick, children, letter, artistCount, active }) => {
    return (
        <div>
            <div style = {{ display: 'flex', justifyContent: 'space-around', width: '100%' }} >
                <ListItem
                    rightIcon={active ? 'keyboard_arrow_down' : 'keyboard_arrow_right'}
                    onClick = {onClick}
                >
                    <div>
                        <Avatar
                            style = {{ backgroundColor: active ? 'deeppink' : 'grey' }}
                            title = {letter}
                        />
                    </div>
                </ListItem>
            </div>
            <div>
                { children }
            </div>
        </div>
    );
};

LetterView.propTypes = {
    active     : PropTypes.bool,
    artistCount: PropTypes.number,
    children   : PropTypes.node,
    letter     : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default LetterView;
